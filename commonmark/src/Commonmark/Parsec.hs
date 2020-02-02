{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Parsec
  ( -- Re-exports
    ParsecT
  , runParserT
  , runParser
  , parse
  , (<|>)
  , (<?>)
  , try
  , getState
  , putState
  , modifyState
  , getPosition
  , setPosition
  , many
  , module Text.Parsec.Pos
  , module Text.Parsec.Combinator
  , module Text.Parsec.Error
  -- Modified and new functions
  , anyChar
  , satisfy
  , char
  , textWhile1
  , string
  , whitespace
  , lineEnd
  , spaceChars
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
import           Control.Monad   (mzero)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Parsec hiding (anyChar, char, string, satisfy)
import           Text.Parsec.Pos
import           Text.Parsec.Combinator
import           Text.Parsec.Error

-- | Parses a single Char satisfying a predicate. We define
-- this here instead of using parsec's default because parsec
-- assumes an 8-space tab stop and gets source pos wrong.
satisfy :: (Monad m, Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
satisfy f = tokenPrim (:[]) updatePos matcher
  where matcher t | f t       = Just t
                  | otherwise = Nothing
        updatePos spos c _ = updatePosWithChar spos c
{-# INLINEABLE satisfy #-}

updatePosWithChar :: SourcePos -> Char -> SourcePos
updatePosWithChar !spos '\t' = incSourceColumn spos
                                  (4 - (sourceColumn spos - 1) `mod` 4)
updatePosWithChar !spos '\n' = incSourceLine spos 1
updatePosWithChar !spos _    = incSourceColumn spos 1
{-# INLINEABLE updatePosWithChar #-}

updatePosWithText :: Text -> SourcePos -> SourcePos
updatePosWithText t pos = T.foldl' updatePosWithChar pos t
{-# INLINEABLE updatePosWithText #-}

-- | Parses any Char.
anyChar :: (Monad m, Stream s m Char) => ParsecT s u m Char
anyChar = satisfy (const True)
{-# INLINEABLE anyChar #-}

-- | Parses a specific Char.
char :: (Monad m, Stream s m Char) => Char -> ParsecT s u m Char
char c = satisfy (== c)
{-# INLINEABLE char #-}

-- | Efficiently parse a text string.
string :: (Monad m) => Text -> ParsecT Text u m Text
string t = do
  inp <- getInput
  if t `T.isPrefixOf` inp
     then do
       setInput $ T.drop (T.length t) inp
       pos <- getPosition
       setPosition $ updatePosWithText t pos
       return $! t
     else mzero
{-# INLINEABLE string #-}

-- | Efficiently parse a chunk of text meeting a condition.
textWhile1 :: Monad m => (Char -> Bool) -> ParsecT Text u m Text
textWhile1 f = do
  inp <- getInput
  pos <- getPosition
  _ <- satisfy f
  let (ts, rest) = T.span f inp
  setInput rest
  setPosition $ updatePosWithText ts pos
  return $! ts
{-# INLINEABLE textWhile1 #-}

-- | Parses one or more whitespace 'Tok's.
whitespace ::  (Monad m) => ParsecT Text u m Text
whitespace = textWhile1 iswhite
  where iswhite ' '  = True
        iswhite '\t' = True
        iswhite '\r' = True
        iswhite '\n' = True
        iswhite _    = False
{-# INLINEABLE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  Monad m => ParsecT Text s m Text
lineEnd =
  (char '\r' >> (("\r\n" <$ char '\n') <|> (return "\r")))
   <|> ("\n" <$ char '\n')
{-# INLINEABLE lineEnd #-}

-- | Parses a 'Spaces' token.
spaceChars :: Monad m => ParsecT Text s m Text
spaceChars = textWhile1 isspace
  where isspace ' '  = True
        isspace '\t' = True
        isspace _    = False
{-# INLINEABLE spaceChars #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: (Monad m) => Int -> ParsecT Text u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINEABLE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: (Monad m) => Int -> ParsecT Text u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINEABLE gobbleUpToSpaces #-}

gobble' :: (Monad m) => Bool -> Int -> ParsecT Text u m Int
gobble' requireAll numspaces
  | numspaces < 1  = return 0
  | otherwise = (do char ' '
                    (+1) <$> gobble' requireAll (numspaces - 1))
               <|>
                (do pos <- getPosition
                    char '\t'
                    pos' <- getPosition
                    case sourceColumn pos' - sourceColumn pos of
                         n | n < numspaces  -> (+ n) <$> gobble' requireAll (numspaces - n)
                           | n == numspaces -> return $! n
                           | otherwise      -> do
                               let newtok = T.replicate (n - numspaces) " "
                               getInput >>= setInput . (newtok <>)
                               setPosition $ incSourceColumn pos numspaces
                               return $! numspaces)
               <|> if requireAll
                      then mzero
                      else return 0
{-# INLINEABLE gobble' #-}

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: Monad m => ParsecT Text u m a -> ParsecT Text u m (a, Text)
withRaw parser = do
  inp <- getInput
  res <- parser
  inp' <- getInput
  case T.stripSuffix inp' inp of
    Just raw -> return $! (res, raw)
    Nothing  -> mzero
{-# INLINEABLE withRaw #-}

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m => ParsecT Text u m ()
nonindentSpaces = () <$ gobbleUpToSpaces 3
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
skipWhile :: (Monad m, Stream s m Char) => (Char -> Bool) -> ParsecT s u m ()
skipWhile f = skipMany (satisfy f)
{-# INLINEABLE skipWhile #-}

-- | Parse optional spaces and an endline.
blankLine :: Monad m => ParsecT Text u m ()
blankLine = try $ do
  skipWhile (\c -> case c of
                     ' '  -> True
                     '\t' -> True
                     _    -> False)
  () <$ lineEnd
{-# INLINEABLE blankLine #-}

-- | Efficiently parse the remaining text on a line,
-- return it plus the source position of the line end
-- (if there is one).
restOfLine :: Monad m => ParsecT Text u m (Text, SourcePos)
restOfLine =
   (do ts <- textWhile1 (\c -> c /= '\r' && c /= '\n')
       pos <- getPosition
       le <- option "" lineEnd
       return $! (ts <> le, pos))
  <|>
   (do pos <- getPosition
       le <- lineEnd
       return $! (le, pos))
{-# INLINEABLE restOfLine #-}
