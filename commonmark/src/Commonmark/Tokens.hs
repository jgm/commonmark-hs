{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE Strict #-}

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
import           Data.List       (foldl')
import           Text.Parsec.Pos

data Tok = Tok { tokType     :: !TokType
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

-- | Convert a 'Text' into a list of 'Tok'. The first parameter
-- species the source name.
tokenize :: String -> Text -> [Tok]
tokenize name = go (initialPos name) . T.unpack
  where
    go _pos [] = []
    go pos  (c:cs) =
          case c of
            ' ' -> let (sps,rest) = span (==' ') cs
                   in  Tok Spaces pos (T.pack (' ':sps)) :
                        go (incSourceColumn pos (1 + length sps)) rest
            '\t' -> Tok Spaces pos "\t" :
                    go (incSourceColumn pos (4 - (sourceColumn pos - 1) `mod` 4)) cs
            '\r' -> case cs of
                      ('\n':cs')
                        -> Tok LineEnd pos "\r\n" :
                           go (incSourceLine (setSourceColumn pos 1) 1) cs'
                      _ -> Tok LineEnd pos "\r" :
                           go (incSourceLine (setSourceColumn pos 1) 1) cs
            '\n' -> Tok LineEnd pos "\n" :
                    go (incSourceLine (setSourceColumn pos 1) 1) cs
            _ | isAlphaNum c ->
                  let (ws,rest) = span isAlphaNum cs
                  in  Tok WordChars pos (T.pack (c : ws)) :
                      go (incSourceColumn pos (1 + length ws)) rest
              | isSpace c    ->
                  Tok UnicodeSpace pos (T.singleton c) : go (incSourceColumn pos 1) cs
              | otherwise    ->
                  Tok (Symbol c) pos (T.singleton c) :
                  go (incSourceColumn pos 1) cs

-- | Reverses 'tokenize'.  @untokenize . tokenize ""@ should be
-- the identity.
untokenize :: [Tok] -> Text
untokenize = foldl' mappend mempty . map tokContents
