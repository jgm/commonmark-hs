{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Parsec
  ( -- Re-exports
    ParsecT
  , runParserT
  , Parsec
  , runParser
  , parse
  , mkPT
  , Consumed(..)
  , Reply(..)
  , (<|>)
  , (<?>)
  , try
  , getParserState
  , setParserState
  , getInput
  , setInput
  , getState
  , putState
  , updateState
  , getPosition
  , setPosition
  , many
  , skipMany
  , module Text.Megaparsec
  -- Modified and new functions
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
import           Text.Megaparsec
import Debug.Trace

setTabWidth :: Int -> ParsecT s u m ()
setTabWidth n = do
  updateParserState $ \st ->
    st{ statePosState = (statePosState st){ pstateTabWidth = Pos n } }


updatePosWithChar :: SourcePos -> Char -> SourcePos
updatePosWithChar !spos '\t' = incSourceColumn spos
                                  (4 - (sourceColumn spos - 1) `mod` 4)
updatePosWithChar !spos '\n' = setSourceColumn (incSourceLine spos 1) 1
updatePosWithChar !spos _    = incSourceColumn spos 1
{-# INLINEABLE updatePosWithChar #-}

updatePosWithText :: Text -> SourcePos -> SourcePos
updatePosWithText t pos = T.foldl' updatePosWithChar pos t
{-# INLINEABLE updatePosWithText #-}

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
  ("\n" <$ char '\n')
   <|> (char '\r' >> (("\r\n" <$ char '\n') <|> (return "\r")))
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
                         n | n < numspaces  ->
                               (+ n) <$> gobble' requireAll (numspaces - n)
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
skipWhile :: (Monad m, Stream s) => (Char -> Bool) -> ParsecT s u m ()
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
