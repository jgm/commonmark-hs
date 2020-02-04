{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Commonmark.Inlines
  ( mkInlineParser
  , defaultInlineParsers
  , IPState(..)
  , InlineParser
  , FormattingSpec(..)
  , defaultFormattingSpecs
  , BracketedSpec(..)
  , defaultBracketedSpecs
  , imageSpec
  , linkSpec
  , pLinkLabel
  , pLinkDestination
  , pLinkTitle
  , pEscaped
  , processEmphasis
  , processBrackets
  -- * Basic parsers
  , pWords
  , pSpaces
  , pSoftbreak
  , pEscapedChar
  , pEntity
  , pBacktickSpan
  , normalizeCodeSpan
  , pCodeSpan
  , pHtml
  , pAutolink
  , pSymbol
  , withAttributes
  )
where

import           Commonmark.Tag             (htmlTag)
import           Commonmark.Parsec
import           Commonmark.ReferenceMap
import           Commonmark.Types
import           Control.Monad              (guard, mzero, when)
import           Data.List                  (foldl')
import           Data.Char                  (isAscii, isLetter, isSpace,
                                             isSymbol, isPunctuation,
                                             isAlphaNum)
import           Data.Dynamic               (Dynamic, toDyn)
import qualified Data.IntMap.Strict         as IntMap
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust, mapMaybe)
import qualified Data.Set                   as Set
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                ((<>))
#endif
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Commonmark.Entity          (unEntity, charEntity, numEntity)

import Debug.Trace

mkInlineParser :: (Monad m, IsInline a)
               => [BracketedSpec a]
               -> [FormattingSpec a]
               -> [InlineParser m a]
               -> [InlineParser m Attributes]
               -> ReferenceMap
               -> [((SourcePos, SourcePos), Text)]
               -> m (Either ParseError a)
mkInlineParser bracketedSpecs formattingSpecs ilParsers attrParsers rm toks = do
  let (positionList, ts) = unzip toks
  let positions = V.fromList positionList
  let attrParser = choice attrParsers
  res <- {-# SCC parseChunks #-} parseChunks
           bracketedSpecs formattingSpecs ilParsers attrParser
           rm positions (T.stripEnd $! T.concat ts)
  return $!
    case res of
       Left err     -> Left err
       Right chunks ->
         (Right .
          unChunks .
          processEmphasis positions .
          processBrackets bracketedSpecs positions rm) chunks

defaultInlineParsers :: (Monad m, IsInline a) => [InlineParser m a]
defaultInlineParsers =
                [ {-# SCC pWords #-} pWords
                , {-# SCC pSpaces #-} pSpaces
                , {-# SCC pSoftbreak #-} pSoftbreak
                , {-# SCC pCodeSpan #-} withAttributes pCodeSpan
                , {-# SCC pEscapedChar #-} pEscapedChar
                , {-# SCC pEntity #-} pEntity
                , {-# SCC pAutolink #-} withAttributes pAutolink
                , {-# SCC pHtml #-} pHtml
                ]

unChunks :: IsInline a => [Chunk a] -> a
unChunks = {-# SCC unChunks #-} foldl' mappend mempty . go
    where
      go []     = []
      go (c:cs) =
        let (f, rest) =
             case cs of
               (Chunk (AddAttributes attrs) _pos _ts : ds) ->
                 (addAttributes attrs, ds)
               _ -> (id, cs) in
        case chunkType c of
          AddAttributes _ -> go rest
          Delim{} -> x : go rest
              where !x = f (ranged range (str t))
                    t = (chunkText c)
                    range = SourceRange
                             [(chunkPos c,
                               incSourceColumn (chunkPos c) (T.length t))]
          Parsed ils -> x : go rest
              where !x = f ils


parseChunks :: (Monad m, IsInline a)
            => [BracketedSpec a]
            -> [FormattingSpec a]
            -> [InlineParser m a]
            -> InlineParser m Attributes
            -> ReferenceMap
            -> V.Vector (SourcePos, SourcePos)
            -> Text
            -> m (Either ParseError [Chunk a])
parseChunks bspecs specs ilParsers attrParser rm positions t =
  runParserT
     (do case positions V.!? 0 of
           Just (p,_) -> setPosition p
           _          -> return ()
         -- build maps of preceding characters and backtick spans
         inp <- getInput
         pos <- getPosition
         pBuildMaps
         updateState $ \st -> st{
           backtickSpans = IntMap.map reverse $ backtickSpans st }
         setInput inp
         setPosition pos
         many (pChunk specmap attrParser ilParsers isDelimChar) <* eof)
     IPState{ backtickSpans = mempty,
              userState = toDyn (),
              linePositions = positions,
              ipReferenceMap = rm,
              precedingChars = mempty,
              attributeParser = attrParser }
     "source" t
  where
   pBuildMaps = {-# SCC pBuildMaps #-} skipMany $ do
      spos <- getPosition
      !c <- anyChar
      when (c == '`') $ do
         len <- ((+ 1) . length) <$> many (char '`')
         updateState $ \st -> spos `seq`
           st{ backtickSpans = IntMap.alter
                (\x -> case x of
                        Nothing -> Just [spos]
                        Just ps -> Just (spos:ps)) len $
                backtickSpans st }
      pos <- getPosition
      !d <- option '\n' $ lookAhead anyChar
      when (isDelimChar d) $ pos `seq`
         updateState $ \st -> st{ precedingChars = M.insert pos c $
                                     precedingChars st }

   isDelimChar c = c `Set.member` delimcharset
   delimcharset = Set.fromList delimchars
   delimchars = '[' : ']' : suffixchars ++
                  prefixchars ++ M.keys specmap
   specmap = mkFormattingSpecMap specs
   prefixchars = mapMaybe bracketedPrefix bspecs
   suffixchars = mapMaybe bracketedSuffixEnd bspecs

data Chunk a = Chunk
     { chunkType :: !(ChunkType a)
     , chunkPos  :: !SourcePos
     , chunkText :: !Text
     } deriving Show

data ChunkType a =
       Delim{ delimType     :: !Char
            , delimCanOpen  :: !Bool
            , delimCanClose :: !Bool
            , delimLength   :: !Int
            , delimSpec     :: !(Maybe (FormattingSpec a))
            }
     | Parsed !a
     | AddAttributes !Attributes
     deriving Show

data IPState m = IPState
     { backtickSpans        :: !(IntMap.IntMap [SourcePos])
                               -- record of lengths of
                               -- backtick spans so we don't scan in vain
     , userState            :: !Dynamic
     , linePositions        :: !(V.Vector (SourcePos, SourcePos))
     , ipReferenceMap       :: !ReferenceMap
     , precedingChars       :: !(M.Map SourcePos Char)
     , attributeParser      :: ParsecT Text (IPState m) m Attributes
     }

type InlineParser m = ParsecT Text (IPState m) m

--- Formatting specs:

-- ^ Specifies delimiters for formatting, e.g. strong emphasis.
data FormattingSpec il = FormattingSpec
    { formattingDelimChar     :: !Char
                              -- ^ Character that triggers formatting
    , formattingIntraWord     :: !Bool
                              -- ^ True if formatting can start/end in a word
    , formattingIgnorePunctuation :: !Bool
                              -- ^ Treat punctuation like letters for
                              -- purposes of computing can open/can close
    , formattingSingleMatch   :: !(Maybe (il -> il))
                              -- ^ Constructor to use for text between
                              -- single delimiters.
    , formattingDoubleMatch   :: !(Maybe (il -> il))
                              -- ^ Constructor to use for text between
                              -- double delimiters.
    , formattingWhenUnmatched :: !Char -- ^ Fallback when not matched.
    }

instance Show (FormattingSpec il) where
  show _ = "<FormattingSpec>"

type FormattingSpecMap il = M.Map Char (FormattingSpec il)

defaultFormattingSpecs :: IsInline il => [FormattingSpec il]
defaultFormattingSpecs =
  [ FormattingSpec '*' True False (Just emph) (Just strong) '*'
  , FormattingSpec '_' False False (Just emph) (Just strong) '_'
  ]

mkFormattingSpecMap :: [FormattingSpec il] -> FormattingSpecMap il
mkFormattingSpecMap fs = M.fromList [(formattingDelimChar s, s) | s <- fs]

--- Bracketed specs:

-- ^ Defines an inline element between square brackets.
data BracketedSpec il = BracketedSpec
     { bracketedName      :: !Text  -- ^ Name of bracketed text type.
     , bracketedNests     :: !Bool  -- ^ True if this can be nested.
     , bracketedPrefix    :: !(Maybe Char) -- ^ Prefix character.
     , bracketedSuffixEnd :: !(Maybe Char) -- ^ Suffix character.
     , bracketedSuffix    :: ReferenceMap
                          -> Text
                          -> Parsec Text () (il -> il)
                          -- ^ Parser for suffix after
                          -- brackets.  Returns a constructor.
                          -- Second parameter is the raw key.
     }

instance Show (BracketedSpec il) where
  show s = "<BracketedSpec " ++ show (bracketedName s) ++ ">"

-- It's important that specs with prefix chars come first:
defaultBracketedSpecs :: IsInline il
                      => [BracketedSpec il]
defaultBracketedSpecs =
  [ imageSpec
  , linkSpec
  ]

linkSpec :: IsInline il => BracketedSpec il
linkSpec = BracketedSpec
           { bracketedName = "Link"
           , bracketedNests = False  -- links don't nest inside links
           , bracketedPrefix = Nothing
           , bracketedSuffixEnd = Just ')'
           , bracketedSuffix = pLinkSuffix
           }

imageSpec :: IsInline il => BracketedSpec il
imageSpec = BracketedSpec
            { bracketedName = "Image"
            , bracketedNests = True
            , bracketedPrefix = Just '!'
            , bracketedSuffixEnd = Just ')'
            , bracketedSuffix = pImageSuffix
            }

pLinkSuffix :: IsInline il
            => ReferenceMap -> Text -> Parsec Text s (il -> il)
pLinkSuffix rm key = do
  LinkInfo target title attrs <- pLink rm key
  return $! addAttributes attrs . link target title

pImageSuffix :: IsInline il
             => ReferenceMap -> Text -> Parsec Text s (il -> il)
pImageSuffix rm key = do
  LinkInfo target title attrs <- pLink rm key
  return $! addAttributes attrs . image target title

---

pChunk :: (IsInline a, Monad m)
       => FormattingSpecMap a
       -> InlineParser m Attributes
       -> [InlineParser m a]
       -> (Char -> Bool)
       -> InlineParser m (Chunk a)
pChunk specmap attrParser ilParsers isDelimChar =
 do pos <- getPosition
    (res, t) <- {-# SCC attrParser #-} withRaw (AddAttributes <$> attrParser)
                 <|>
                {-# SCC pInline #-} (\(x,t) -> (Parsed x,t)) <$>
                   pInline ilParsers isDelimChar
    return $! Chunk res pos t
  <|> {-# SCC pDelimChunk #-} pDelimChunk specmap isDelimChar

pDelimChunk :: (IsInline a, Monad m)
            => FormattingSpecMap a
            -> (Char -> Bool)
            -> InlineParser m (Chunk a)
pDelimChunk specmap isDelimChar = do
  c <- satisfy isDelimChar
  pos <- (\x -> incSourceColumn x (-1)) <$> getPosition
  let !mbspec = M.lookup c specmap
  cs <- if isJust mbspec
           then many $ char c
           else return []
  let toks = T.pack (c:cs)
  st <- getState
  next <- option '\n' (lookAhead anyChar)
  let precedingChar = M.lookup pos (precedingChars st)
  let precededByWhitespace = maybe True isSpace precedingChar
  let precededByPunctuation =
       case formattingIgnorePunctuation <$> mbspec of
         Just True -> False
         _         -> maybe False (\d -> isSymbol d || isPunctuation d)
                        precedingChar
  let followedByWhitespace = isSpace next
  let followedByPunctuation =
       case formattingIgnorePunctuation <$> mbspec of
         Just True -> False
         _         -> not followedByWhitespace &&
                      (isSymbol next || isPunctuation next)
  let leftFlanking = not followedByWhitespace &&
         (not followedByPunctuation ||
          precededByWhitespace ||
          precededByPunctuation)
  let rightFlanking = not precededByWhitespace &&
         (not precededByPunctuation ||
          followedByWhitespace ||
          followedByPunctuation)
  let !canOpen =
         leftFlanking &&
          (maybe True formattingIntraWord mbspec ||
           not rightFlanking ||
           precededByPunctuation)
  let !canClose =
         rightFlanking &&
          (maybe True formattingIntraWord mbspec ||
           not leftFlanking ||
           followedByPunctuation)
  let toks' = case mbspec of
                    Nothing -> toks
                    -- change tokens to unmatched fallback
                    -- this is mainly for quotes
                    Just spec
                      | formattingWhenUnmatched spec /= c ->
                         T.map (\_ -> formattingWhenUnmatched spec) toks
                    _ -> toks
  let !len = T.length toks'
  return $! Chunk Delim{ delimType = c
                       , delimCanOpen = canOpen
                       , delimCanClose = canClose
                       , delimSpec = mbspec
                       , delimLength = len
                       } pos toks'

withAttributes :: (IsInline a, Monad m) => InlineParser m a -> InlineParser m a
withAttributes p = do
  x <- p
  attrParser <- attributeParser <$> getState
  option x $ (\attr -> addAttributes attr x) <$> attrParser

pInline :: (IsInline a, Monad m)
        => [InlineParser m a]
        -> (Char -> Bool)
        -> InlineParser m (a, Text)
pInline ilParsers isDelimChar = do
  (xs, ts) <- withRaw $ many1
               (do startpos <- getPosition
                   res <- choice ilParsers <|> pSymbol isDelimChar
                   endpos <- getPosition
                   positions <- linePositions <$> getState
                   let range = rangeFromStartEnd positions startpos endpos
                   return (ranged range res))
  return $! (mconcat xs, ts)

rangeFromStartEnd :: V.Vector (SourcePos, SourcePos)
                  -> SourcePos
                  -> SourcePos
                  -> SourceRange
rangeFromStartEnd positions startpos endpos = SourceRange realPositions
 where
  startline = sourceLine startpos
  endline = sourceLine endpos
  realPos lnum = case positions V.!? (lnum - 1) of
                   Nothing -> error $ "Could not find position for line " ++
                                    show lnum
                   Just (start, end) ->
                     let scol = if lnum == startline
                                   then sourceColumn startpos +
                                        sourceColumn start
                                   else sourceColumn start
                         ecol = if lnum == endline
                                   then sourceColumn endpos +
                                        sourceColumn start
                                   else sourceColumn end
                         slin = sourceLine start
                         elin = sourceLine end
                         sn = sourceName startpos
                     in  (newPos sn slin scol, newPos sn elin ecol)
  realPositions = map realPos [startline..endline]

pEscapedChar :: (IsInline a, Monad m) => InlineParser m a
pEscapedChar = do
  char '\\'
  option (str "\\") $
    (escapedChar <$> satisfy isAsciiSymbol)
     <|> (lineBreak <$ (lineEnd <* optional whitespace))

pEntity :: (IsInline a, Monad m) => InlineParser m a
pEntity = try $ do
  char '&'
  ent <- numEntity <|> charEntity
  return $! entity ("&" <> ent)

pCodeSpan :: (IsInline a, Monad m) => InlineParser m a
pCodeSpan =
  pBacktickSpan >>=
  \case
    Left ticks     -> return $! str ticks
    Right codetoks -> return $! code . normalizeCodeSpan $ codetoks

pBacktickSpan :: Monad m
              => InlineParser m (Either Text Text)
pBacktickSpan = do
  t <- textWhile1 (== '`')
  pos' <- getPosition
  let numticks = T.length t
  st' <- getState
  case dropWhile (<= pos') <$> IntMap.lookup numticks (backtickSpans st') of
     Just (pos'':ps) -> do
          codetoks <- many $ do
            pos <- getPosition
            guard $ pos < pos''
            anyChar
          backticks <- textWhile1 (== '`')
          guard $ T.length backticks == numticks
          updateState $ \st ->
            st{ backtickSpans = IntMap.insert numticks ps (backtickSpans st) }
          return $! Right $ T.pack codetoks
     _ -> return $! Left t

normalizeCodeSpan :: Text -> Text
normalizeCodeSpan = removeSurroundingSpace . T.map nltosp
  where
   nltosp '\n' = ' '
   nltosp c    = c
   removeSurroundingSpace s
     | not (T.null s)
     , not (T.all (== ' ') s)
     , T.head s == ' '
     , T.last s == ' ' = T.drop 1 $ T.dropEnd 1 s
     | otherwise = s

pHtml :: (IsInline a, Monad m) => InlineParser m a
pHtml = try $ do
  _ <- char '<'
  rawInline (Format "html") . ("<" <>) <$> htmlTag

pAutolink :: (IsInline a, Monad m) => InlineParser m a
pAutolink = try $ do
  _ <- char '<'
  (target, lab) <- pUri <|> pEmail
  _ <- char '>'
  return $! link target "" (str lab)

pUri :: Monad m => InlineParser m (Text, Text)
pUri = try $ do
  s <- pScheme
  _ <- char ':'
  let isURIChar c = not (isSpace c) && c  /= '<' && c /= '>'
  t <- textWhile1 isURIChar
  let uri = s <> ":" <> t
  return $! (uri, uri)

pScheme :: Monad m => InlineParser m Text
pScheme = do
  _ <- lookAhead $ satisfy (\c -> isAscii c && isLetter c)
  t <- textWhile1 (\c -> (isAscii c && isAlphaNum c) ||
                              c == '+' || c == '.' || c == '-')
  let len = T.length t
  guard $ len >= 2 && len <= 32
  return $! t

pEmail :: Monad m => InlineParser m (Text, Text)
pEmail = do
  let isEmailSymbolChar c =
         c == '.' || c == '!' || c == '#' || c == '$' || c == '%' ||
         c == '&' || c == '\'' || c == '*' || c == '+' || c == '/' ||
         c == '=' || c == '?' || c == '^' || c == '_' || c == '`' ||
         c == '{' || c == '|' || c == '}' || c == '~' || c == '-' ||
         c == ']'
  lookAhead $ satisfy (\c -> isAscii c && isAlphaNum c)
  name <- textWhile1 (\c -> (isAscii c && isAlphaNum c) ||
                            isEmailSymbolChar c)
  _ <- char '@'
  let domainPart = do
        lookAhead $ satisfy (\c -> isAscii c && isAlphaNum c)
        x <- textWhile1 (\c -> isAscii c && isAlphaNum c)
        xs <- many $ (do char '-'
                         notFollowedBy eof
                         notFollowedBy (char '.')
                         return "-")
                    <|> textWhile1 (\c -> isAscii c && isAlphaNum c)
        return $! x <> mconcat xs
  d <- domainPart
  ds <- many (char '.' >> domainPart)
  let addr = name <> "@" <> T.intercalate "." (d:ds)
  return $! ("mailto:" <> addr, addr)

pSpaces :: (IsInline a, Monad m) => InlineParser m a
pSpaces = do
  t <- textWhile1 isSpaceChar
  (do lineEnd
      optional whitespace
      (mempty <$ eof) <|>
        if T.length t > 1
           then return $! lineBreak
           else return $! softBreak)
   <|> (return $! str t)

pSoftbreak :: (IsInline a, Monad m) => InlineParser m a
pSoftbreak = do
  _ <- lineEnd
  optional whitespace
  (mempty <$ eof) <|> return softBreak

pWords :: (IsInline a, Monad m) => InlineParser m a
pWords = do
  t <- textWhile1 isAlphaNum
  return $! str t

pSymbol :: (IsInline a, Monad m)
        => (Char -> Bool)  -- ^ Test for delimiter character
        -> InlineParser m a
pSymbol isDelimChar = do
  c <- satisfy (not . isDelimChar)
  return $! str (T.singleton c)

data DState a = DState
     { leftCursor     :: !(Cursor (Chunk a))
     , rightCursor    :: !(Cursor (Chunk a))
     , refmap         :: !ReferenceMap
     , stackBottoms   :: !(M.Map Text SourcePos)
     , absoluteBottom :: !SourcePos
     , dLinePositions :: !(V.Vector (SourcePos, SourcePos))
     }

processEmphasis :: IsInline a
                => V.Vector (SourcePos, SourcePos)
                -> [Chunk a]
                -> [Chunk a]
processEmphasis positions xs =
  case break (\case
               (Chunk Delim{} _ _) -> True
               _ -> False) xs of
       (_,[]) -> xs
       (ys,z:zs) ->
           let startcursor = Cursor (Just z) (reverse ys) zs
           in  processEm DState{ leftCursor = startcursor
                               , rightCursor = startcursor
                               , refmap = emptyReferenceMap
                               , stackBottoms = mempty
                               , absoluteBottom = chunkPos z
                               , dLinePositions = positions }

-- prettyCursors :: (IsInline a) => Cursor (Chunk a) -> Cursor (Chunk a) -> String
-- prettyCursors left right =
--   toS (reverse $ befores left) <> (maybe "" (inBrs . toS . (:[])) (center left)) <>
--   if (chunkPos <$> center left) == (chunkPos <$> center right)
--      then toS (afters right)
--      else toS (middles) <> (maybe "" (inBrs . toS . (:[])) (center right)) <>
--           toS (afters right)
--  where middles = take (length (afters left) - length (afters right) -
--                          maybe 0 (const 1) (center right)) (afters left)
--        toS = show . unChunks
--        inBrs x = "{" ++ x ++ "}"

processEm :: IsInline a => DState a -> [Chunk a]
processEm st =
  let left = leftCursor st
      right = rightCursor st
      bottoms = stackBottoms st
      positions = dLinePositions st
  in case -- trace (prettyCursors left right)
          (center left, center right) of
       (_, Nothing) -> reverse $
                         case center (rightCursor st) of
                            Nothing -> befores (rightCursor st)
                            Just c  -> c : befores (rightCursor st)

       (Nothing, Just (Chunk Delim{ delimType = c
                                  , delimCanClose = True } pos t)) ->
           processEm
           st{ leftCursor   = right
             , rightCursor  = moveRight right
             , stackBottoms = M.insert
                   (T.pack (c : show (T.length t `mod` 3))) pos
                   $ stackBottoms st
             }

       (Nothing, Just _) -> processEm
           st{ leftCursor = right
             , rightCursor = moveRight right
             }

       (Just chunk, Just closedelim@(Chunk Delim{ delimType = c,
                                                  delimCanClose = True,
                                                  delimSpec = Just spec}
                                           closePos t))
         | delimsMatch chunk closedelim ->
           let closelen = T.length t
               opendelim = chunk
               contents = takeWhile (\ch -> chunkPos ch /= closePos)
                          (afters left)
               openlen = T.length (chunkText opendelim)
               fallbackConstructor x = str (T.singleton c) <> x <>
                                       str (T.singleton c)
               (constructor, numtoks) =
                case (formattingSingleMatch spec, formattingDoubleMatch spec) of
                        (_, Just c2)
                          | min openlen closelen >= 2 -> (c2, 2)
                        (Just c1, _)     -> (c1, 1)
                        _                -> (fallbackConstructor, 1)
               (openrest, opentoks) =
                 T.splitAt (openlen - numtoks) (chunkText opendelim)
               (closetoks, closerest) =
                 T.splitAt numtoks (chunkText closedelim)
               addnewopen = if T.null openrest
                               then id
                               else (opendelim{ chunkText = openrest } :)
               addnewclose = if T.null closerest
                                then id
                                else (closedelim{ chunkText = closerest } :)
               emphtoks = opentoks <> T.concat (map chunkText contents) <>
                          closetoks
               emphStartPos = incSourceColumn (chunkPos opendelim)
                                 (T.length openrest)
               emphEndPos = incSourceColumn (chunkPos closedelim)
                                 (T.length closetoks)
               newelt = Chunk
                         (Parsed $
                           ranged
                             (rangeFromStartEnd positions
                                emphStartPos emphEndPos) $
                             constructor $ unChunks contents)
                         (chunkPos chunk)
                         emphtoks
               newcursor = Cursor (Just newelt)
                              (addnewopen (befores left))
                              (addnewclose (afters right))
           in processEm
              st{ rightCursor = moveRight newcursor
                , leftCursor = newcursor
                }

         | Just (chunkPos chunk) <=
             M.lookup (T.pack (c: show (T.length t `mod` 3))) bottoms ->
                  processEm
                  st{ leftCursor   = right
                    , rightCursor  = moveRight right
                    , stackBottoms =  M.insert
                        (T.pack (c : show (T.length t `mod` 3)))
                        (chunkPos closedelim)
                        $ stackBottoms st
                    }

         | otherwise -> processEm st{ leftCursor = moveLeft left }

       _ -> processEm
            st{ rightCursor = moveRight right
              , leftCursor  = moveRight left }
{-# SCC processEm #-}

-- This only applies to emph delims, not []:
delimsMatch :: IsInline a
            => Chunk a -> Chunk a -> Bool
delimsMatch (Chunk open@Delim{} p1 _)
            (Chunk close@Delim{} p2 _) =
  delimCanOpen open && delimCanClose close &&
      (delimType open == delimType close &&
           if (delimCanOpen open && delimCanClose open) ||
                (delimCanOpen close && delimCanClose close)
                then delimLength close `mod` 3 == 0 ||
                     (delimLength open + delimLength close) `mod` 3 /= 0
                else True) &&
    p2 > p1
delimsMatch _ _ = False

processBrackets :: IsInline a
                => [BracketedSpec a]
                -> V.Vector (SourcePos, SourcePos)
                -> ReferenceMap
                -> [Chunk a]
                -> [Chunk a]
processBrackets bracketedSpecs positions rm xs =
  case break (\case
               (Chunk Delim{ delimType = '[' } _ _) -> True
               _ -> False) xs of
       (_,[]) -> xs
       (ys,z:zs) ->
          let  startcursor = Cursor (Just z) (reverse ys) zs
          in   processBs bracketedSpecs
                 DState{ leftCursor = startcursor
                       , rightCursor = startcursor
                       , refmap = rm
                       , stackBottoms = mempty
                       , absoluteBottom = chunkPos z
                       , dLinePositions = positions
                       }

data Cursor a = Cursor
     { center  :: Maybe a
     , befores :: [a]
     , afters  :: [a]
     }
     deriving Show

moveLeft :: Cursor a -> Cursor a
moveLeft (Cursor Nothing  []     zs) = Cursor Nothing  [] zs
moveLeft (Cursor Nothing  (x:xs) zs) = Cursor (Just x) xs zs
moveLeft (Cursor (Just x) []     zs) = Cursor Nothing  [] (x:zs)
moveLeft (Cursor (Just x) (y:ys) zs) = Cursor (Just y) ys (x:zs)
{-# INLINE moveLeft #-}

moveRight :: Cursor a -> Cursor a
moveRight (Cursor Nothing zs  [])     = Cursor Nothing  zs     []
moveRight (Cursor Nothing zs  (x:xs)) = Cursor (Just x) zs     xs
moveRight (Cursor (Just x) zs [])     = Cursor Nothing  (x:zs) []
moveRight (Cursor (Just x) zs (y:ys)) = Cursor (Just y) (x:zs) ys
{-# INLINE moveRight #-}

processBs :: IsInline a
          => [BracketedSpec a] -> DState a -> [Chunk a]
processBs bracketedSpecs st =
  let left = leftCursor st
      right = rightCursor st
      bottoms = stackBottoms st
      bottom = absoluteBottom st
      positions = dLinePositions st
  -- trace (prettyCursors left right) $ return $! ()
  in case (center left, center right) of
       (_, Nothing) -> reverse $
                         case center (rightCursor st) of
                            Nothing -> befores (rightCursor st)
                            Just c  -> c : befores (rightCursor st)

       (Nothing, Just chunk) ->
          processBs bracketedSpecs
                       st{ leftCursor = moveRight right
                         , rightCursor = moveRight right
                         , absoluteBottom = chunkPos chunk
                         }

       (Just chunk, Just chunk')
         | chunkPos chunk < bottom ->
            processBs bracketedSpecs
                       st { leftCursor = moveRight right
                          , rightCursor = moveRight right
                          , absoluteBottom = chunkPos chunk'
                          }

       (Just opener@(Chunk Delim{ delimCanOpen = True, delimType = '[' } _ _),
        Just closer@(Chunk Delim{ delimCanClose = True, delimType = ']'} closePos _)) ->
          let chunksinside = takeWhile (\ch -> chunkPos ch /= closePos)
                               (afters left)
              isBracket (Chunk Delim{ delimType = c' } _ _) =
                 c' == '[' || c' == ']'
              isBracket _ = False
              key = if any isBracket chunksinside
                       then ""
                       else
                         case T.concat (map chunkText chunksinside) of
                              ks | T.length ks <= 999 -> ks
                              _  -> ""
              prefixChar = case befores left of
                                 Chunk Delim{delimType = c} _ x : _
                                   | not (T.null x) -> Just c
                                 _  -> Nothing
              rm = refmap st

              specs = [s | s <- bracketedSpecs
                         , case bracketedPrefix s of
                                Just c  -> Just c == prefixChar
                                Nothing -> True
                         , maybe True  (< chunkPos opener)
                            (M.lookup (bracketedName s) bottoms) ]

          in case parse
                 (withRaw
                   (do setPosition (incSourceColumn closePos 1)
                       (spec, constructor) <- choice $
                           map (\s -> (s,) <$> bracketedSuffix s rm key)
                           specs
                       pos <- getPosition
                       return (spec, constructor, pos)))
                 ""
                 (mconcat (map chunkText (afters right))) of
                   Left _ -> -- match but no link/image
                         processBs bracketedSpecs
                            st{ leftCursor = moveLeft (leftCursor st)
                              , rightCursor = fixSingleQuote $
                                    moveRight (rightCursor st) }
                   Right ((spec, constructor, newpos), desttoks) ->
                     let left' = case bracketedPrefix spec of
                                      Just _  -> moveLeft left
                                      Nothing -> left
                         openers = case bracketedPrefix spec of
                                        Just _ -> maybe id (:) (center left')
                                                   [opener]
                                        Nothing -> [opener]
                         openerPos = case openers of
                                          (x:_) -> chunkPos x
                                          _     -> chunkPos opener
                         elttoks = T.concat (map chunkText
                                     (openers ++ chunksinside ++ [closer]))
                                      <> desttoks
                         elt = ranged (rangeFromStartEnd positions
                                         openerPos newpos)
                                  $ constructor $ unChunks
                                  $ processEmphasis positions chunksinside
                         eltchunk = Chunk (Parsed elt) openerPos elttoks
                         afterchunks = dropWhile ((< newpos) . chunkPos)
                                         (afters right)
                     in case afterchunks of
                           []     -> processBs bracketedSpecs
                                      st{ rightCursor = Cursor Nothing
                                          (eltchunk : befores left') [] }
                           (y:ys) ->
                             let lbs = befores left'
                             in processBs bracketedSpecs st{
                                  leftCursor =
                                    Cursor (Just eltchunk) lbs (y:ys)
                                , rightCursor = fixSingleQuote $
                                    Cursor (Just y) (eltchunk:lbs) ys
                                , stackBottoms =
                                    -- if a link, we need to ensure that
                                    -- nothing matches as link containing it
                                    if bracketedNests spec
                                       then stackBottoms st
                                       else M.insert (bracketedName spec)
                                            (chunkPos opener)
                                            $ stackBottoms st
                                }


       (_, Just (Chunk Delim{ delimCanClose = True, delimType = ']' } _ _))
          -> processBs bracketedSpecs st{ leftCursor = moveLeft left }

       (Just _, Just (Chunk Delim{ delimCanOpen = True, delimType = '[' } _ _))
          -> processBs bracketedSpecs
                st{ leftCursor = right
                  , rightCursor = moveRight right }

       (_, _) -> processBs bracketedSpecs
                st{ rightCursor = moveRight right }
{-# SCC processBs #-}


-- This just changes a single quote Delim that occurs
-- after ) or ] so that canOpen = False.  This is an ad hoc
-- way to prevent "[a]'s dog'" from being parsed wrong.
-- Ideally there'd be a way to put this restriction in
-- the FormattingSpec for smart ', but currently there
-- isn't.
fixSingleQuote :: Cursor (Chunk a) -> Cursor (Chunk a)
fixSingleQuote
  (Cursor (Just (Chunk d@(Delim{ delimType = '\'' }) pos toks)) xs ys) =
  Cursor (Just (Chunk d{ delimCanOpen = False } pos toks)) xs ys
fixSingleQuote cursor = cursor

pLink :: ReferenceMap -> Text -> Parsec Text s LinkInfo
pLink rm key = do
  pInlineLink <|> pReferenceLink rm key

pInlineLink :: Monad m => ParsecT Text s m LinkInfo
pInlineLink = try $ do
  _ <- char '('
  optional whitespace
  target <- unEntity <$> pLinkDestination
  optional whitespace
  title <- option "" $
             unEntity <$> (pLinkTitle <* optional whitespace)
  _ <- char ')'
  return $! LinkInfo { linkDestination = target
                    , linkTitle = title
                    , linkAttributes = mempty }

pLinkDestination :: Monad m => ParsecT Text s m Text
pLinkDestination = T.pack <$> (pAngleDest <|> pNormalDest 0)
  where
    pAngleDest = do
      _ <- char '<'
      res <- many $ satisfy (\c -> c /= '<' && c /= '>' && c /= '\\' &&
                                  c /= '\r' && c /= '\n')
                   <|> pEscaped
      _ <- char '>'
      return res

    pNormalDest (numparens :: Int) = do
      res <- pNormalDest' numparens
      if null res
         then res <$ lookAhead (char ')')
         else return res

    pNormalDest' numparens
     | numparens > 32 = mzero
     | otherwise = (do
          c <- satisfy (\case
                          '\\' -> True
                          ')'  -> numparens >= 1
                          ' '  -> False
                          '\t' -> False
                          '\r' -> False
                          '\n' -> False
                          _    -> True)
          case c of
            '\\' -> do
              c' <- option c $ satisfy isAsciiSymbol
              (c':) <$> pNormalDest' numparens
            '('  -> (c:) <$> pNormalDest' (numparens + 1)
            ')'  -> (c:) <$> pNormalDest' (numparens - 1)
            _    -> (c:) <$> pNormalDest' numparens)
          <|> ([] <$ guard (numparens == 0))

-- parses backslash + escapable character, or just backslash
pEscaped :: Monad m => ParsecT Text s m Char
pEscaped = do
  bs <- char '\\'
  option bs $ satisfy isAsciiSymbol <|> char '\r' <|> char '\n'

isAsciiSymbol :: Char -> Bool
isAsciiSymbol c = isAscii c && (isSymbol c || isPunctuation c)

pLinkTitle :: Monad m => ParsecT Text s m Text
pLinkTitle = inbetween '"' '"' <|> inbetween '\'' '\'' <|> inbetween '(' ')'

inbetween :: Monad m => Char -> Char -> ParsecT Text s m Text
inbetween op cl = try $ do
  _ <- char op
  xs <- many (pEscaped <|> satisfy (\c -> c /= op && c /= cl))
  _ <- char cl
  return $ T.pack xs

pLinkLabel :: Monad m => ParsecT Text s m Text
pLinkLabel = try $ do
  lab <- snd <$> (try
         (between (char '[') (char ']')
           (withRaw
             (many (pEscaped <|> satisfy (\c -> c /= ']' && c /= '['))))))
  guard $ T.length lab <= 999
  return lab

pReferenceLink :: ReferenceMap -> Text -> Parsec Text s LinkInfo
pReferenceLink rm key = do
  lab <- option key pLinkLabel
  let key' = if T.null lab
                then key
                else lab
  maybe mzero return $! lookupReference key' rm

isSpaceChar :: Char -> Bool
isSpaceChar '\t' = True
isSpaceChar ' '  = True
isSpaceChar _    = False
