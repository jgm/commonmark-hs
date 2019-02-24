{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Commonmark.Inlines
  ( mkInlineParser
  , defaultInlineParsers
  , IPState(..)
  , InlineParser
  , FormattingSpec(..)
  , defaultFormattingSpecs
  , BracketedSpec(..)
  , defaultBracketedSpecs
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
  , pCodeSpan
  , pHtml
  , pAutolink
  , pSymbol
  )
where

import           Commonmark.Tag             (htmlTag)
import           Commonmark.Tokens
import           Commonmark.Util
import           Commonmark.ReferenceMap
import           Commonmark.Types
import           Control.Monad              (guard, mzero)
import           Data.Char                  (isAscii, isLetter)
import           Data.Dynamic               (Dynamic)
import qualified Data.IntMap.Strict         as IntMap
import qualified Data.Map                   as M
import           Data.Maybe                 (isJust, mapMaybe)
import qualified Data.Set                   as Set
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid                ((<>))
#endif
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Commonmark.Entity          (unEntity, charEntity, numEntity)
import           Text.Parsec                hiding (State, space)
import           Text.Parsec.Pos

mkInlineParser :: (Monad m, IsInline a)
               => [BracketedSpec a]
               -> [FormattingSpec a]
               -> [InlineParser m a]
               -> ReferenceMap
               -> [Tok]
               -> m (Either ParseError a)
mkInlineParser bracketedSpecs formattingSpecs ilParsers rm toks = do
  let iswhite t = hasType Spaces t || hasType LineEnd t
  res <- parseChunks bracketedSpecs formattingSpecs ilParsers rm
         (dropWhile iswhite . reverse . dropWhile iswhite . reverse $ toks)
  return $
    case res of
       Left err     -> Left err
       Right chunks ->
         (Right .
          unChunks .
          processEmphasis .
          processBrackets bracketedSpecs rm) chunks

defaultInlineParsers :: (Monad m, IsInline a) => [InlineParser m a]
defaultInlineParsers =
                [ pWords
                , pSpaces
                , pSoftbreak
                , pEscapedChar
                , pEntity
                , pCodeSpan
                , pHtml
                , pAutolink
                ]

unChunks :: IsInline a => [Chunk a] -> a
unChunks = mconcat . map unChunk

unChunk :: IsInline a => Chunk a -> a
unChunk chunk =
  case chunkType chunk of
       Delim{} -> ranged range (str (untokenize ts))
                   where ts = chunkToks chunk
                         range =
                           case ts of
                                []    -> mempty
                                (_:_) -> SourceRange
                                           [(chunkPos chunk,
                                             incSourceColumn
                                               (tokPos (last ts)) 1)]
       Parsed ils -> ils

parseChunks :: (Monad m, IsInline a)
            => [BracketedSpec a]
            -> [FormattingSpec a]
            -> [InlineParser m a]
            -> ReferenceMap
            -> [Tok]
            -> m (Either ParseError [Chunk a])
parseChunks _ _ _ _ []             = return (Right [])
parseChunks bspecs specs ilParsers rm (t:ts) =
  runParserT (setPosition (tokPos t) >> many (pChunk specmap ilParsers) <* eof)
          IPState{ afterPunct = initialPos "",
                   afterSpace = tokPos t,
                   backtickSpans = getBacktickSpans (t:ts),
                   userState = undefined,
                   formattingDelimChars = Set.fromList $
                     '[' : ']' : suffixchars ++ prefixchars
                                  ++ M.keys specmap,
                   ipReferenceMap = rm }
          "source" (t:ts)
  where specmap = mkFormattingSpecMap specs
        prefixchars = mapMaybe bracketedPrefix bspecs
        suffixchars = mapMaybe bracketedSuffixEnd bspecs

data Chunk a = Chunk
     { chunkType :: ChunkType a
     , chunkPos  :: !SourcePos
     , chunkToks :: [Tok]
     } deriving Show

data ChunkType a =
       Delim{ delimType     :: !Char
            , delimCanOpen  :: !Bool
            , delimCanClose :: !Bool
            , delimLength   :: !Int
            , delimSpec     :: Maybe (FormattingSpec a)
            }
     | Parsed a
     deriving Show

data IPState = IPState
     { afterPunct           :: SourcePos -- pos of next token after punctuation
     , afterSpace           :: SourcePos  -- pos of next token after space
     , backtickSpans        :: IntMap.IntMap [SourcePos]
                               -- record of lengths of
                               -- backtick spans so we don't scan in vain
     , userState            :: Dynamic
     , formattingDelimChars :: Set.Set Char
     , ipReferenceMap       :: ReferenceMap
     } deriving Show

type InlineParser m = ParsecT [Tok] IPState m

--- Formatting specs:

-- ^ Specifies delimiters for formatting, e.g. strong emphasis.
data FormattingSpec il = FormattingSpec
    { formattingDelimChar     :: !Char
                              -- ^ Character that triggers formatting
    , formattingIntraWord     :: !Bool
                              -- ^ True if formatting can start/end in a word
    , formattingSingleMatch   :: Maybe (il -> il)
                              -- ^ Constructor to use for text between
                              -- single delimiters.
    , formattingDoubleMatch   :: Maybe (il -> il)
                              -- ^ Constructor to use for text between
                              -- double delimiters.
    , formattingWhenUnmatched :: !Char -- ^ Fallback when not matched.
    }

instance Show (FormattingSpec il) where
  show _ = "<FormattingSpec>"

type FormattingSpecMap il = M.Map Char (FormattingSpec il)

defaultFormattingSpecs :: IsInline il => [FormattingSpec il]
defaultFormattingSpecs =
  [ FormattingSpec '*' True (Just emph) (Just strong) '*'
  , FormattingSpec '_' False (Just emph) (Just strong) '_'
  ]

mkFormattingSpecMap :: [FormattingSpec il] -> FormattingSpecMap il
mkFormattingSpecMap fs = M.fromList [(formattingDelimChar s, s) | s <- fs]

--- Bracketed specs:

-- ^ Defines an inline element between square brackets.
data BracketedSpec il = BracketedSpec
     { bracketedName      :: !Text  -- ^ Name of bracketed text type.
     , bracketedNests     :: !Bool  -- ^ True if this can be nested.
     , bracketedPrefix    :: Maybe Char -- ^ Prefix character.
     , bracketedSuffixEnd :: Maybe Char -- ^ Suffix character.
     , bracketedSuffix    :: ReferenceMap
                          -> Text
                          -> Parsec [Tok] () (il -> il)
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
            => ReferenceMap -> Text -> Parsec [Tok] s (il -> il)
pLinkSuffix rm key = do
  LinkInfo target title <- pLink rm key
  return $ link target title

pImageSuffix :: IsInline il
             => ReferenceMap -> Text -> Parsec [Tok] s (il -> il)
pImageSuffix rm key = do
  LinkInfo target title <- pLink rm key
  return $ image target title

---

-- Construct a map of n-length backtick spans, with source positions,
-- so we can avoid scanning forward when it will be fruitless.
getBacktickSpans :: [Tok] -> IntMap.IntMap [SourcePos]
getBacktickSpans = go 0 (initialPos "")
  where
    go :: Int -> SourcePos -> [Tok] -> IntMap.IntMap [SourcePos]
    go n pos []
     | n > 0     = IntMap.singleton n [pos]
     | otherwise = IntMap.empty
    go n pos (t:ts) =
     case tokType t of
       Symbol '`'
         | n > 0     -> go (n+1) pos ts
         | otherwise -> go (n+1) (tokPos t) ts
       _ | n > 0     -> IntMap.alter (\x ->
                            case x of
                                 Nothing -> Just [pos]
                                 Just ps -> Just (pos:ps)) n $ go 0 pos ts
         | otherwise -> go 0 pos ts

pChunk :: (IsInline a, Monad m)
       => FormattingSpecMap a
       -> [InlineParser m a]
       -> InlineParser m (Chunk a)
pChunk specmap ilParsers =
      (do pos <- getPosition
          (ils, ts) <- unzip <$> many1 (pInline ilParsers)
          return $ Chunk (Parsed (mconcat ils)) pos (mconcat ts))
   <|> pDelimChunk specmap

pDelimTok :: Monad m => InlineParser m Tok
pDelimTok = do
  delimChars <- formattingDelimChars <$> getState
  satisfyTok (\case
               Tok (Symbol c) _ _ -> Set.member c delimChars
               _ -> False)

pNonDelimTok :: Monad m => InlineParser m Tok
pNonDelimTok = do
  delimChars <- formattingDelimChars <$> getState
  satisfyTok (\case
               Tok (Symbol c) _ _ -> Set.notMember c delimChars
               _ -> True)

pDelimChunk :: (IsInline a, Monad m)
            => FormattingSpecMap a
            -> InlineParser m (Chunk a)
pDelimChunk specmap = try $ do
  tok@(Tok (Symbol c) pos _) <- pDelimTok
  let mbspec = M.lookup c specmap
  more <- if isJust mbspec
             then many $ symbol c
             else return []
  let toks = tok:more
  newpos <- getPosition
  st <- getState
  next <- (tokType <$> lookAhead anyTok) <|> return LineEnd
  let precededByWhitespace = afterSpace st == pos
  let precededByPunctuation = afterPunct st == pos
  let followedByWhitespace = next == Spaces ||
                             next == LineEnd ||
                             next == UnicodeSpace
  let followedByPunctuation = not followedByWhitespace &&
                              next /= WordChars
  let leftFlanking = not followedByWhitespace &&
         (not followedByPunctuation ||
          precededByWhitespace ||
          precededByPunctuation)
  let rightFlanking = not precededByWhitespace &&
         (not precededByPunctuation ||
          followedByWhitespace ||
          followedByPunctuation)
  let canOpen =
         leftFlanking &&
          (maybe True formattingIntraWord mbspec ||
           not rightFlanking ||
           precededByPunctuation)
  let canClose =
         rightFlanking &&
          (maybe True formattingIntraWord mbspec ||
           not leftFlanking ||
           followedByPunctuation)
  updateState $ \s -> s{ afterPunct = newpos }
  let toks' = case mbspec of
                    Nothing -> toks
                    -- change tokens to unmatched fallback
                    -- this is mainly for quotes
                    Just spec
                      | formattingWhenUnmatched spec /= c ->
                         map (\t -> t{ tokContents =
                               T.map (\_ -> formattingWhenUnmatched spec)
                                  (tokContents t) }) toks
                    _ -> toks
  return $ Chunk Delim{ delimType = c
                      , delimCanOpen = canOpen
                      , delimCanClose = canClose
                      , delimSpec = mbspec
                      , delimLength = length toks'
                      } pos toks'

pInline :: (IsInline a, Monad m)
        => [InlineParser m a]
        -> InlineParser m (a, [Tok])
pInline ilParsers = do
  (res, toks) <- withRaw $ choice ilParsers <|> pSymbol
  newpos <- getPosition
  case tokType (last toks) of
       Spaces       -> updateState $ \st -> st{ afterSpace = newpos }
       UnicodeSpace -> updateState $ \st -> st{ afterSpace = newpos }
       LineEnd      -> updateState $ \st -> st{ afterSpace = newpos }
       Symbol _     -> updateState $ \st -> st{ afterPunct = newpos }
       _            -> return ()
  return (ranged (rangeFromToks toks) res, toks)

rangeFromToks :: [Tok] -> SourceRange
rangeFromToks = SourceRange . go
 where go ts =
        case break (hasType LineEnd) ts of
             ([], [])     -> []
             ([], _:ys)   -> go ys
             (x:xs, [])   ->
                let y = last (x:xs) in
                [(tokPos x,
                  incSourceColumn (tokPos y) (T.length (tokContents y)))]
             (x:_, y:ys) ->
                (tokPos x, tokPos y) : go ys

pEscapedChar :: (IsInline a, Monad m) => InlineParser m a
pEscapedChar = do
  symbol '\\'
  (do Tok (Symbol c) _ _ <- satisfyTok asciiSymbol
      return $ escapedChar c)
   <|>
   (lineBreak <$ lineEnd)
   <|>
   return (str "\\")

pEntity :: (IsInline a, Monad m) => InlineParser m a
pEntity = try $ do
  symbol '&'
  ent <- numEntity <|> charEntity
  return (entity ("&" <> untokenize ent))

pBacktickSpan :: Monad m
              => InlineParser m (Either [Tok] [Tok])
pBacktickSpan = do
  ts <- many1 (symbol '`')
  pos' <- getPosition
  let numticks = length ts
  st' <- getState
  case dropWhile (<= pos') <$> IntMap.lookup numticks (backtickSpans st') of
     Just (pos'':ps) -> do
          codetoks <- many $ satisfyTok (\tok -> tokPos tok < pos'')
          backticks <- many $ satisfyTok (hasType (Symbol '`'))
          guard $ length backticks == numticks
          updateState $ \st ->
            st{ backtickSpans = IntMap.insert numticks ps (backtickSpans st) }
          return $ Right codetoks
     _ -> return $ Left ts

pCodeSpan :: (IsInline a, Monad m) => InlineParser m a
pCodeSpan =
  pBacktickSpan >>=
  \case
    Left ticks     -> return $ str (untokenize ticks)
    Right codetoks -> return $ code . normalizeCodeSpan . untokenize
                             $ codetoks
    where normalizeCodeSpan = removeSurroundingSpace . T.map nltosp
          nltosp '\n' = ' '
          nltosp c    = c
          removeSurroundingSpace s
             | not (T.null s)
             , T.head s == ' '
             , T.last s == ' ' = T.drop 1 $ T.dropEnd 1 s
             | otherwise = s

pHtml :: (IsInline a, Monad m) => InlineParser m a
pHtml = try $ do
  t <- symbol '<'
  rawInline (Format "html") . untokenize . (t:) <$> htmlTag

pAutolink :: (IsInline a, Monad m) => InlineParser m a
pAutolink = try $ do
  symbol '<'
  (target, lab) <- pUri <|> pEmail
  symbol '>'
  return $ link target "" (str lab)

pUri :: Monad m => InlineParser m (Text, Text)
pUri = try $ do
  s <- pScheme
  _ <- symbol ':'
  let isURITok t =
       case tokType t of
            Spaces     -> False
            LineEnd    -> False
            (Symbol c) -> c > ' ' && c /= '<' && c /= '>'
            _          -> True
  ts <- many $ satisfyTok isURITok
  let uri = s <> ":" <> untokenize ts
  return (uri, uri)

pScheme :: Monad m => InlineParser m Text
pScheme = try $ do
  t <- satisfyWord (\t -> case T.uncons t of
                               Nothing -> False
                               Just (c,rest) -> isAscii c && isLetter c &&
                                                T.all isAscii rest)
  ts <- many $ oneOfToks [WordChars, Symbol '+', Symbol '.', Symbol '-']
  let s = untokenize (t:ts)
  let len = T.length s
  guard $ len >= 2 && len <= 32
  return s

pEmail :: Monad m => InlineParser m (Text, Text)
pEmail = try $ do
  let isEmailSymbolTok (Tok (Symbol c) _ _) =
         c == '.' || c == '!' || c == '#' || c == '$' || c == '%' ||
         c == '&' || c == '\'' || c == '*' || c == '+' || c == '/' ||
         c == '=' || c == '?' || c == '^' || c == '_' || c == '`' ||
         c == '{' || c == '|' || c == '}' || c == '~' || c == '-' ||
         c == ']'
      isEmailSymbolTok _ = False
  name <- many1 $ satisfyWord (T.all isAscii)
               <|> satisfyTok isEmailSymbolTok
  _ <- symbol '@'
  let domainPart = do
        x <- satisfyWord (T.all isAscii)
        xs <- many $ (symbol '-' <* notFollowedBy eof <* notFollowedBy (symbol '.'))
                  <|> satisfyWord (T.all isAscii)
        return (x:xs)
  d <- domainPart
  ds <- many (symbol '.' >> domainPart)
  let addr = untokenize name <> "@" <> T.intercalate "." (map untokenize (d:ds))
  return ("mailto:" <> addr, addr)

pSpaces :: (IsInline a, Monad m) => InlineParser m a
pSpaces = do
  Tok Spaces pos t <- satisfyTok (hasType Spaces)
  (do Tok LineEnd pos' _ <- satisfyTok (hasType LineEnd)
      case sourceColumn pos' - sourceColumn pos of
           n | n >= 2 ->
             return lineBreak
           _ ->
             return softBreak)
   <|> return (str t)

pSoftbreak :: (IsInline a, Monad m) => InlineParser m a
pSoftbreak = softBreak <$ satisfyTok (hasType LineEnd)

pWords :: (IsInline a, Monad m) => InlineParser m a
pWords = do
  t <- satisfyTok (hasType WordChars)
  return $ str (tokContents t)

{-
getWord :: [Tok] -> [Tok]
getWord (t1@Tok{ tokType = Spaces } : t2@Tok{ tokType = WordChars } : rest) =
  t1:t2:getWord rest
getWord (t1@Tok{ tokType = WordChars } : rest) =
  t1:getWord rest
getWord _ = []
-}
  
pSymbol :: (IsInline a, Monad m) => InlineParser m a
pSymbol = str . tokContents <$> pNonDelimTok

data DState a = DState
     { leftCursor     :: Cursor (Chunk a)
     , rightCursor    :: Cursor (Chunk a)
     , refmap         :: ReferenceMap
     , stackBottoms   :: M.Map Text SourcePos
     , absoluteBottom :: SourcePos
     }

processEmphasis :: IsInline a => [Chunk a] -> [Chunk a]
processEmphasis xs =
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
                               , absoluteBottom = chunkPos z }

{- for debugging:
prettyCursors :: (IsInline a) => Cursor (Chunk a) -> Cursor (Chunk a) -> String
prettyCursors left right =
  toS (reverse $ befores left) <> (maybe "" (inBrs . toS . (:[])) (center left)) <>
  if (chunkPos <$> center left) == (chunkPos <$> center right)
     then toS (afters right)
     else toS (middles) <> (maybe "" (inBrs . toS . (:[])) (center right)) <>
          toS (afters right)
 where middles = take (length (afters left) - length (afters right) -
                         maybe 0 (const 1) (center right)) (afters left)
       toS = show . unChunks
       inBrs x = "{" ++ x ++ "}"
-}

processEm :: IsInline a => DState a -> [Chunk a]
processEm st =
  let left = leftCursor st
      right = rightCursor st
      bottoms = stackBottoms st
      -- trace (prettyCursors left right) $ return $! ()
  in case (center left, center right) of
       (_, Nothing) -> reverse $
                         case center (rightCursor st) of
                            Nothing -> befores (rightCursor st)
                            Just c  -> c : befores (rightCursor st)

       (Nothing, Just (Chunk Delim{ delimType = c
                                  , delimCanClose = True } pos ts)) ->
           processEm
           st{ leftCursor   = right
             , rightCursor  = moveRight right
             , stackBottoms = M.insert
                   (T.pack (c : show (length ts `mod` 3))) pos
                   $ stackBottoms st
             }

       (Nothing, Just _) -> processEm
           st{ leftCursor = right
             , rightCursor = moveRight right
             }

       (Just chunk, Just closedelim@(Chunk Delim{ delimType = c,
                                                  delimCanClose = True,
                                                  delimSpec = Just spec} _ ts))
         | delimsMatch chunk closedelim ->
           let closelen = length ts
               opendelim = chunk
               contents = take
                 (length (afters left) - (length (afters right) + 1))
                 (afters left)
               openlen = length (chunkToks opendelim)
               fallbackConstructor x = str (T.singleton c) <> x <>
                                       str (T.singleton c)
               (constructor, numtoks) =
                case (formattingSingleMatch spec, formattingDoubleMatch spec) of
                        (_, Just c2)
                          | min openlen closelen >= 2 -> (c2, 2)
                        (Just c1, _)     -> (c1, 1)
                        _                -> (fallbackConstructor, 1)
               (openrest, opentoks) =
                 splitAt (openlen - numtoks) (chunkToks opendelim)
               (closetoks, closerest) =
                 splitAt numtoks (chunkToks closedelim)
               addnewopen = if null openrest
                               then id
                               else (opendelim{ chunkToks = openrest } :)
               addnewclose = if null closerest
                                then id
                                else (closedelim{ chunkToks = closerest } :)
               emphtoks = opentoks ++ concatMap chunkToks contents ++ closetoks
               newelt = Chunk
                         (Parsed $
                           ranged (rangeFromToks emphtoks) $
                             constructor $ mconcat $
                                map unChunk contents)
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
             M.lookup (T.pack (c: show (length ts `mod` 3))) bottoms ->
                  processEm
                  st{ leftCursor   = right
                    , rightCursor  = moveRight right
                    , stackBottoms =  M.insert
                        (T.pack (c : show (length ts `mod` 3)))
                        (chunkPos closedelim)
                        $ stackBottoms st
                    }

         | otherwise -> processEm st{ leftCursor = moveLeft left }

       _ -> processEm
            st{ rightCursor = moveRight right
              , leftCursor  = moveRight left }

-- This only applies to emph delims, not []:
delimsMatch :: IsInline a
            => Chunk a -> Chunk a -> Bool
delimsMatch (Chunk open@Delim{} _ opents) (Chunk close@Delim{} _ closets) =
  delimCanOpen open && delimCanClose close &&
      (delimType open == delimType close &&
           if (delimCanOpen open && delimCanClose open) ||
                (delimCanOpen close && delimCanClose close)
                then (delimLength open + delimLength close) `mod` 3 /= 0
                else True) &&
    opents /= closets
delimsMatch _ _ = False

processBrackets :: IsInline a
                => [BracketedSpec a] -> ReferenceMap -> [Chunk a] -> [Chunk a]
processBrackets bracketedSpecs rm xs =
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
        Just closer@(Chunk Delim{ delimCanClose = True, delimType = ']'} _ _)) ->
          let chunksinside = take
                           (length (afters left) - (length (afters right) + 1))
                           (afters left)
              isBracket (Chunk Delim{ delimType = c' } _ _) =
                 c' == '[' || c' == ']'
              isBracket _ = False
              key = if any isBracket chunksinside
                       then ""
                       else
                         case untokenize (concatMap chunkToks chunksinside) of
                              ks | T.length ks <= 999 -> ks
                              _  -> ""
              prefixChar = case befores left of
                                 Chunk Delim{delimType = c} _ [_] : _
                                    -> Just c
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
                   (do (spec, constructor) <- choice $
                           map (\s -> (s,) <$> bracketedSuffix s rm key)
                           specs
                       pos <- getPosition
                       return (spec, constructor, pos)))
                 ""
                 (mconcat (map chunkToks (afters right))) of
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
                         elttoks = concatMap chunkToks
                                     (openers ++ chunksinside ++ [closer])
                                      ++ desttoks
                         elt = ranged (rangeFromToks elttoks)
                                  $ constructor $ unChunks $
                                       processEmphasis chunksinside
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

pLink :: ReferenceMap -> Text -> Parsec [Tok] s LinkInfo
pLink rm key = do
  pInlineLink <|> pReferenceLink rm key

pInlineLink :: Parsec [Tok] s LinkInfo
pInlineLink = try $ do
  _ <- symbol '('
  optional whitespace
  target <- unEntity <$> pLinkDestination
  optional whitespace
  title <- option "" $
             unEntity <$> (pLinkTitle <* optional whitespace)
  _ <- symbol ')'
  return $ LinkInfo { linkDestination = target, linkTitle = title }

pLinkDestination :: Parsec [Tok] s [Tok]
pLinkDestination = pAngleDest <|> pNormalDest
  where
    pAngleDest = try $ do
      _ <- symbol '<'
      res <- many (noneOfToks [Symbol '<', Symbol '>', Symbol '\\',
                                LineEnd] <|> pEscaped)
      _ <- symbol '>'
      return res
    pNormalDest = try $ do
      res <- many $
                  many1 (satisfyTok (\case
                       Tok (Symbol c) _ _ -> c /= '(' && c /= ')' && c /= '\\'
                       Tok Spaces _ _     -> False
                       Tok LineEnd _ _    -> False
                       _ -> True)
                    <|> pEscaped)
                 <|> (do op <- symbol '('
                         mid <- pNormalDest
                         cl <- symbol ')'
                         return (op : mid ++ [cl]))
      return $ concat res

-- parses backslash + escapable character, or just backslash
pEscaped :: Monad m => ParsecT [Tok] s m Tok
pEscaped = do
  bs <- symbol '\\'
  option bs $ satisfyTok asciiSymbol <|> lineEnd

asciiSymbol :: Tok -> Bool
asciiSymbol (Tok (Symbol c) _ _) = isAscii c
asciiSymbol _                    = False

pLinkTitle :: Parsec [Tok] s [Tok]
pLinkTitle = inbetween '"' '"' <|> inbetween '\'' '\'' <|> inbetween '(' ')'

inbetween :: Char -> Char -> Parsec [Tok] s [Tok]
inbetween op cl =
  try $ between (symbol op) (symbol cl)
     (many (pEscaped <|> noneOfToks [Symbol cl]))

pLinkLabel :: Monad m => ParsecT [Tok] s m Text
pLinkLabel = try $ do
  lab <- untokenize
      <$> try (between (symbol '[') (symbol ']')
            (snd <$> withRaw (many
              (pEscaped <|> noneOfToks [Symbol ']', Symbol '[']))))
  guard $ T.length lab <= 999
  return lab

pReferenceLink :: ReferenceMap -> Text -> Parsec [Tok] s LinkInfo
pReferenceLink rm key = do
  lab <- option key pLinkLabel
  let key' = if T.null lab
                then key
                else lab
  maybe mzero return $ lookupReference key' rm

