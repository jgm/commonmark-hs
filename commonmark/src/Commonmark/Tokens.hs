{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Commonmark.Tokens
  ( Tok(..)
  , TokType(..)
  , tokenize
  , untokenize
  ) where

import           Data.Char       (isAlphaNum, isSpace)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Data       (Data, Typeable)
import           Commonmark.ParserCombinators
                                 (SourcePos(..), initialPos,
                                  HasSourcePos(..))

data Tok = Tok { tokType     :: TokType
               , tokPos      :: SourcePos
               , tokContents :: Text
               }
               deriving (Show, Eq, Data, Typeable)

data TokType =
       Spaces
     | UnicodeSpace
     | LineEnd
     | WordChars
     | Symbol !Char
     deriving (Show, Eq, Ord, Data, Typeable)

instance HasSourcePos Tok where
  tokenSourcePos = tokPos

-- | Convert a 'Text' into a list of 'Tok'. The first parameter
-- species the source name.
tokenize :: String -> Text -> [Tok]
tokenize name = go (initialPos name)
  where
    go pos t =
      case T.uncons t of
        Nothing  -> []
        Just x   ->
          let (tok, newpos, t') = getTok pos x
          in  tok `seq` newpos `seq` t' `seq` (tok : go newpos t')
    getTok pos@(SourcePos n ln col) (c, t) =
      case c of
        ' ' -> let (sps,rest) = T.span (==' ') t
               in  (Tok Spaces pos (T.cons ' ' sps),
                    SourcePos n ln (col + 1 + T.length sps), rest)
        '\t' -> (Tok Spaces pos "\t",
                   SourcePos n ln (col + (4 - (col - 1) `mod` 4)), t)
        '\r' -> case T.uncons t of
                  Just ('\n',t')
                    -> (Tok LineEnd pos "\r\n", SourcePos n (ln+1) 1, t')
                  _ -> (Tok LineEnd pos "\r", SourcePos n (ln+1) 1, t)
        '\n' -> (Tok LineEnd pos "\n", SourcePos n (ln+1) 1, t)
        _
         | isAlphaNum c ->
             let (ws,rest) = T.span isAlphaNum t
             in  (Tok WordChars pos (T.cons c ws),
                  SourcePos n ln (col + 1 + T.length ws), rest)
         | isSpace c    ->
            (Tok UnicodeSpace pos (T.singleton c), SourcePos n ln (col+1), t)
         | otherwise    ->
            (Tok (Symbol c) pos (T.singleton c), SourcePos n ln (col+1), t)

-- | Reverses 'tokenize'.  @untokenize . tokenize ""@ should be
-- the identity.
untokenize :: [Tok] -> Text
untokenize = mconcat . map tokContents
