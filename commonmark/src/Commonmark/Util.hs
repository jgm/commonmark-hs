{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Util
  ( anySymbol
  , whitespace
  , lineEnd
  , gobbleSpaces
  , gobbleUpToSpaces
  , withRaw
  , blankLine
  , restOfLine
  , isOneOfCI
  , nonindentSpaces
  , skipManyTill
  , skipWhile
  )
  where
import           Control.Monad   (mzero, void)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Parsec
import           Text.Parsec.Pos (updatePosString)

-- | Parses any 'Symbol' 'Tok'.
anySymbol :: (Monad m, Stream s m Char) => ParsecT s u m Tok
anySymbol = satisfy (\c -> isSymbol c || isPunctuation c)
{-# INLINEABLE anySymbol #-}

-- | Parses one or more whitespace 'Tok's.
whitespace ::  (Monad m, Stream s m Char) => ParsecT s u m [Char]
whitespace = many1 $ satisfy isWhitespaceChar
  where
   isWhitespaceChar '\t' = True
   isWhitespaceChar ' '  = True
   isWhitespaceChar '\r' = True
   isWhitespaceChar '\n' = True
   isWhitespaceChar _    = False
{-# INLINEABLE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  (Monad m, Stream s m Char) => ParsecT s u m Char
lineEnd = char '\n' <|> (char '\r' *> optional (char '\n'))
{-# INLINEABLE lineEnd #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: (Monad m, Stream s m Char) => Int -> ParsecT s u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINEABLE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: (Monad m, Stream s m Char) => Int -> ParsecT s u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINEABLE gobbleUpToSpaces #-}

gobble' :: (Monad m, Stream s m Char) => Bool -> Int -> ParsecT s u m Int
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
                      (T.replicate (n - numspaces) " ")
               getInput >>= setInput . (newtok:)
               return $! numspaces)
    <|> if requireAll
           then mzero
           else return 0
  | otherwise     = return 0
{-# INLINEABLE gobble' #-}

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: (Monad m, Stream s m Char) => ParsecT s u m a -> ParsecT s u m (a, [Char])
withRaw parser = do
  pst <- getParserState
  res <- parser
  newpos <- getPosition
  setParserState pst
  toks <- many ((getPosition >>= guard (< newpos)) >> anyChar)
  return $! (res, rawtoks)
{-# INLINEABLE withRaw #-}

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: (Monad m, Stream s m Char) => ParsecT [Char] u m ()
nonindentSpaces = void $ gobbleUpToSpaces 3
{-# INLINEABLE nonindentSpaces #-}

-- | Case-insensitive membership in a list of 'Text's.
isOneOfCI :: [Text] -> Text -> Bool
isOneOfCI ts t = T.toLower t `elem` ts
{-# INLINEABLE isOneOfCI #-}

-- | Apply @p@ many times until @stop@ succeeds, discarding results.
skipManyTill :: ParsecT s u m a -> ParsecT s u m b -> ParsecT s u m ()
skipManyTill p stop = scan
    where scan = (() <$ stop) <|> (p >> scan)
{-# INLINEABLE skipManyTill #-}

-- | Efficiently skip 'Tok's satisfying a certain condition.
skipWhile :: (Monad m, Stream s m Char) => (Tok -> Bool) -> ParsecT [Tok] u m ()
skipWhile f = skipMany (satisfyTok f)
{-# INLINEABLE skipWhile #-}

-- | Parse optional spaces and an endline.
blankLine :: (Monad m, Stream s m Char) => ParsecT s u m ()
blankLine = try $ do
  skipWhile (hasType Spaces)
  void lineEnd
{-# INLINEABLE blankLine #-}

-- | Efficiently parse the remaining tokens on a line,
-- return them plus the source position of the line end
-- (if there is one).
restOfLine :: (Monad m, Stream s m Char) => ParsecT s u m ([Tok], SourcePos)
restOfLine = do
  toks <- getInput
  case break (hasType LineEnd) toks of
    (_,[]) -> do
      ts <- many1 anyTok
      pos <- getPosition
      return $! (ts, pos)
    (ts,le@(Tok _ pos _):rest) -> do
      setInput (le:rest)
      lineEnd -- gobble le, so parsec doesn't think nothing consumed
      return (ts ++ [le], pos)
{-# INLINEABLE restOfLine #-}
