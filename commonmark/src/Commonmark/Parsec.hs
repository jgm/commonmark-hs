{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.Parsec
  ( ParsecT
  , Parsec
  , runParserT
  , setTabWidth
  , getState
  , putState
  , updateState
  , incSourceColumn
  , incSourceLine
  , setSourceColumn
  , getPosition
  , setPosition
  , textWhile1
  , anyChar
  , module Text.Megaparsec
  , module Text.Megaparsec.Char
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
  , skipWhile
  )
  where
import           Control.Monad   (mzero)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Text.Megaparsec hiding (ParsecT, Parsec, runParserT)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec as M
import qualified Control.Monad.Trans.State.Strict as S
import           Control.Monad.Trans.Class (lift)
import           Data.Void (Void)
import           Data.Functor.Identity (Identity)
import Debug.Trace

type ParsecT s u m = M.ParsecT Void s (S.StateT u m)

type Parsec s u = ParsecT s u Identity

getState :: Monad m => ParsecT s u m u
getState = lift S.get

runParserT :: Monad m
           => ParsecT s u m a -> u -> String -> s
           -> m (Either (ParseErrorBundle s Void) a)
runParserT parser st sourcename t =
  S.evalStateT (M.runParserT parser sourcename t) st

putState :: Monad m => u -> ParsecT s u m ()
putState s = lift $ S.put s

updateState :: Monad m => (u -> u) -> ParsecT s u m ()
updateState f = lift $ S.modify f

getPosition :: MonadParsec e s m => m SourcePos
getPosition = getSourcePos

setPosition :: MonadParsec e s m => SourcePos -> m ()
setPosition pos = updateParserState $ \st ->
  let ps = statePosState st
   in st{ statePosState = ps{
          pstateSourcePos = pos
       } }

textWhile1 :: (Monad m, Stream s, Tokens s ~ Text, Token s ~ Char)
           => (Char -> Bool) -> ParsecT s u m Text
textWhile1 = takeWhile1P Nothing

anyChar :: (Monad m, Stream s, Token s ~ Char) => ParsecT s u m Char
anyChar = anySingle

setTabWidth :: (Monad m, Stream s) => Int -> ParsecT s u m ()
setTabWidth n = do
  updateParserState $ \st ->
    st{ statePosState = (statePosState st){ pstateTabWidth = mkPos n } }

incSourceColumn :: SourcePos -> Int -> SourcePos
incSourceColumn pos n =
  pos{ sourceColumn = mkPos $ unPos (sourceColumn pos) + n }

setSourceColumn :: SourcePos -> Int -> SourcePos
setSourceColumn pos n =
  pos{ sourceColumn = mkPos n }

incSourceLine :: SourcePos -> Int -> SourcePos
incSourceLine pos n =
  pos{ sourceColumn = mkPos $ unPos (sourceLine pos) + n }

-- | Parses one or more whitespace 'Tok's.
whitespace ::  (Monad m, Stream s, Tokens s ~ Text, Token s ~ Char)
           => ParsecT s u m Text
whitespace = takeWhile1P (Just "whitespace") iswhite
  where iswhite ' '  = True
        iswhite '\t' = True
        iswhite '\r' = True
        iswhite '\n' = True
        iswhite _    = False
{-# INLINEABLE whitespace #-}

-- | Parses a 'LineEnd' token.
lineEnd ::  (Monad m, Stream s, Token s ~ Char)
        => ParsecT s u m Text
lineEnd =
  ("\n" <$ char '\n')
   <|> (char '\r' >> (("\r\n" <$ char '\n') <|> (return "\r")))
{-# INLINEABLE lineEnd #-}

-- | Parses one or more spaces or tabs.
spaceChars :: (Monad m, Stream s, Tokens s ~ Text, Token s ~ Char)
           => ParsecT s u m Text
spaceChars = takeWhile1P (Just "space") isspace
  where isspace ' '  = True
        isspace '\t' = True
        isspace _    = False
{-# INLINEABLE spaceChars #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: Monad m
             => Int -> ParsecT Text u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINEABLE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: Monad m
                 => Int -> ParsecT Text u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINEABLE gobbleUpToSpaces #-}

gobble' :: Monad m => Bool -> Int -> ParsecT Text u m Int
gobble' requireAll numspaces
  | numspaces < 1  = return 0
  | otherwise = (do char ' '
                    (+1) <$> gobble' requireAll (numspaces - 1))
               <|>
                (do pos <- getPosition
                    char '\t'
                    pos' <- getPosition
                    case unPos (sourceColumn pos') -
                         unPos (sourceColumn pos) of
                         n | n < numspaces  ->
                               (+ n) <$> gobble' requireAll (numspaces - n)
                           | n == numspaces -> return $! n
                           | otherwise      -> do
                               let newtok = T.replicate (n - numspaces) " "
                               getInput >>= setInput . (newtok <>)
                               setPosition pos{
                                 sourceColumn = mkPos
                                   (unPos (sourceColumn pos) + numspaces) }
                               return $! numspaces)
               <|> if requireAll
                      then mzero
                      else return 0
{-# INLINEABLE gobble' #-}

-- | Applies a parser and returns its value (if successful)
-- plus a list of the raw tokens parsed.
withRaw :: (Monad m, Stream s, Tokens s ~ Text, Token s ~ Char)
        => ParsecT s u m a -> ParsecT s u m (a, Text)
withRaw parser = do
  (t, res) <- match parser
  return (res, t)
{-# INLINEABLE withRaw #-}

-- | Gobble up to 3 spaces (may be part of a tab).
nonindentSpaces :: Monad m
                => ParsecT Text u m ()
nonindentSpaces = () <$ gobbleUpToSpaces 3
{-# INLINEABLE nonindentSpaces #-}

-- | Case-insensitive membership in a list of 'Text's.
isOneOfCI :: [Text] -> Text -> Bool
isOneOfCI ts t = T.toLower t `elem` ts
{-# INLINEABLE isOneOfCI #-}

-- | Efficiently skip 'Tok's satisfying a certain condition.
skipWhile :: (Monad m, Stream s, Token s ~ Char)
          => (Char -> Bool) -> ParsecT s u m ()
skipWhile f = skipMany (satisfy f)
{-# INLINEABLE skipWhile #-}

-- | Parse optional spaces and an endline.
blankLine :: (Monad m, Stream s, Token s ~ Char)
          => ParsecT s u m ()
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
restOfLine :: (Monad m, Stream s, Tokens s ~ Text, Token s ~ Char)
           => ParsecT s u m (Text, SourcePos)
restOfLine =
   (do ts <- takeWhile1P (Just "cr or lf") (\c -> c /= '\r' && c /= '\n')
       pos <- getPosition
       le <- option "" lineEnd
       return $! (ts <> le, pos))
  <|>
   (do pos <- getPosition
       le <- lineEnd
       return $! (le, pos))
{-# INLINEABLE restOfLine #-}
