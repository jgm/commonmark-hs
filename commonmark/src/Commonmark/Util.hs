{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Util
  ( satisfyTok
  , satisfyWord
  , anyTok
  , anySymbol
  , symbol
  , whitespace
  , lineEnd
  , spaceTok
  , oneOfToks
  , noneOfToks
  , gobbleSpaces
  , gobbleUpToSpaces
  , withRaw
  , hasType
  , textIs
  , blankLine
  , restOfLine
  , isOneOfCI
  , nonindentSpaces
  , skipManyTill
  , skipWhile
  , tokensWhile1
  )
  where
import           Control.Monad   (mzero, void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.UTF8 as UTF8
import           Text.Parsec
import           Text.Parsec.Pos (updatePosString)
import           Data.Char (toLower)
import           Commonmark.Tokens

-- | Parses a single 'Tok' satisfying a predicate.
satisfyTok :: Monad m => (Tok -> Bool) -> ParsecT [Tok] s m Tok
satisfyTok f = tokenPrim (UTF8.toString . tokContents) updatePos matcher
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos :: SourcePos -> Tok -> [Tok] -> SourcePos
        updatePos _spos _ (Tok _ !pos _ : _) = pos
        updatePos !spos (Tok _ _pos !t) []    =
          updatePosString spos (UTF8.toString t)
{-# INLINE satisfyTok #-}

-- | Parses any 'Tok'.
anyTok :: Monad m => ParsecT [Tok] s m Tok
anyTok = satisfyTok (const True)
{-# INLINE anyTok #-}

-- | Parses any 'Symbol' 'Tok'.
anySymbol :: Monad m => ParsecT [Tok] s m Tok
anySymbol = satisfyTok (\t -> case tokType t of
                                    Symbol _ -> True
                                    _        -> False)
{-# INLINE anySymbol #-}

-- | Parses a 'Symbol' with character @c@.
symbol ::  Monad m => Char -> ParsecT [Tok] s m Tok
symbol c = satisfyTok (hasType (Symbol c))
{-# INLINE symbol #-}

-- | Parses a 'Tok' with one of the listed types.
oneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
oneOfToks toktypes = satisfyTok (hasTypeIn toktypes)
{-# INLINE oneOfToks #-}

-- | Parses a 'Tok' with none of the listed types.
noneOfToks ::  Monad m => [TokType] -> ParsecT [Tok] s m Tok
noneOfToks toktypes = satisfyTok (not . hasTypeIn toktypes)
{-# INLINE noneOfToks #-}

-- | Parses one or more whitespace 'Tok's.
whitespace ::  Monad m => ParsecT [Tok] s m [Tok]
whitespace = many1 $ satisfyTok (\t -> case tokType t of
                                         Spaces  -> True
                                         LineEnd -> True
                                         _       -> False)
{-# INLINE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParsecT [Tok] s m Tok
lineEnd = satisfyTok (hasType LineEnd)
{-# INLINE lineEnd #-}

-- | Parses a 'Spaces' token.
spaceTok :: Monad m => ParsecT [Tok] s m Tok
spaceTok = satisfyTok (hasType Spaces)
{-# INLINE spaceTok #-}

-- | Parses a 'WordChars' token matching a predicate.
satisfyWord ::  Monad m => (ByteString -> Bool) -> ParsecT [Tok] s m Tok
satisfyWord f = satisfyTok (\t -> hasType WordChars t && textIs f t)
{-# INLINE satisfyWord #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: Monad m => Int -> ParsecT [Tok] u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: Monad m => Int -> ParsecT [Tok] u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINE gobbleUpToSpaces #-}

gobble' :: Monad m => Bool -> Int -> ParsecT [Tok] u m Int
gobble' requireAll numspaces
  | numspaces >= 1 = (do
    Tok Spaces pos _ <- satisfyTok (hasType Spaces)
    pos' <- getPosition
    case sourceColumn pos' - sourceColumn pos of
         n | n < numspaces  -> (+ n) <$> gobble' requireAll (numspaces - n)
           | n == numspaces -> return $! n
           | otherwise      -> do
               let newtok = Tok Spaces
                      (incSourceColumn pos numspaces)
                      (B8.replicate (n - numspaces) ' ')
               getInput >>= setInput . (newtok:)
               return $! numspaces)
    <|> if requireAll
           then mzero
           else return 0
  | otherwise     = return 0
{-# INLINE gobble' #-}

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: Monad m => ParsecT [Tok] s m a -> ParsecT [Tok] s m (a, [Tok])
withRaw parser = do
  toks <- getInput
  res <- parser
  newpos <- getPosition
  let rawtoks = takeWhile ((< newpos) . tokPos) toks
  return $! (res, rawtoks)
{-# INLINE withRaw #-}

-- | Filters tokens of a certain type.
hasType :: TokType -> Tok -> Bool
hasType ty (Tok ty' _ _) = ty == ty'
{-# INLINE hasType #-}

hasTypeIn :: [TokType] -> Tok -> Bool
hasTypeIn tys (Tok ty' _ _) = ty' `elem` tys

-- | Filters tokens with certain contents.
textIs :: (ByteString -> Bool) -> Tok -> Bool
textIs f (Tok _ _ t) = f t
{-# INLINE textIs #-}

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m => ParsecT [Tok] u m ()
nonindentSpaces = void $ gobbleUpToSpaces 3
{-# INLINE nonindentSpaces #-}

-- | Case-insensitive membership in a list of lowercase ASCII ByteStrings.
isOneOfCI :: [ByteString] -> ByteString -> Bool
isOneOfCI ts t = B8.map toLower t `elem` ts
{-# INLINE isOneOfCI #-}

-- | Apply @p@ many times until @stop@ succeeds, discarding results.
skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
skipManyTill p stop = scan
    where scan = (() <$ stop) <|> (p >> scan)
{-# INLINE skipManyTill #-}

-- | Efficiently skip 'Tok's satisfying a certain condition.
skipWhile :: Monad m => (Tok -> Bool) -> ParsecT [Tok] u m ()
skipWhile f = skipMany (satisfyTok f)
{-# INLINE skipWhile #-}

-- | Parse optional spaces and an endline.
blankLine :: Monad m => ParsecT [Tok] s m ()
blankLine = try $ do
  skipWhile (hasType Spaces)
  void lineEnd
{-# INLINE blankLine #-}

-- | Efficiently parse one or more tokens meeting a
-- condition.
tokensWhile1 :: Monad m => (Tok -> Bool) -> ParsecT [Tok] s m [Tok]
tokensWhile1 f = do
  t <- satisfyTok f
  toks <- getInput
  case span f toks of
    (ts, rest@(Tok _ pos _ : _)) -> do
      setInput rest
      setPosition pos
      return $! (t:ts)
    (ts, []) -> do
      skipWhile f
      return $! (t:ts)

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParsecT [Tok] s m ([Tok], SourcePos)
restOfLine =
   (do ts <- tokensWhile1 (not . hasType LineEnd)
       pos <- getPosition
       le <- option [] $ (:[]) <$> lineEnd
       return $! (ts ++ le, pos))
  <|>
   (do le@(Tok _ pos _) <- lineEnd
       return $! ([le], pos))
{-# INLINE restOfLine #-}
