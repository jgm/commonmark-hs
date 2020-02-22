{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}

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
import           Text.Parsec.Pos

data Tok = Tok { tokType     :: !TokType
               , tokPos      :: !SourcePos
               , tokContents :: !Text
               }
               deriving (Show, Eq, Data, Typeable)

data TokType =
       Spaces
     | UnicodeSpace
     | LineEnd
     | WordChars
     | Symbol {-# UNPACK #-} !Char
     deriving (Show, Eq, Ord, Data, Typeable)

-- | Convert a 'Text' into a list of 'Tok'. The first parameter
-- species the source name.
tokenize :: String -> Text -> [Tok]
tokenize name = go (initialPos name) . T.groupBy f
  where
    -- We group \r\n, consecutive spaces, and consecutive alphanums;
    -- everything else gets in a token by itself.
    f '\r' '\n' = True
    f ' ' ' '   = True
    f x   y     = isAlphaNum x && isAlphaNum y

    go _pos [] = []
    go !pos (t:ts) = -- note that t:ts are guaranteed to be nonempty
      let !thead = T.head t in
      if | thead == ' ' ->
            Tok Spaces pos t :
            go (incSourceColumn pos (T.length t)) ts
         | t == "\t" ->
            Tok Spaces pos t :
            go (incSourceColumn pos (4 - (sourceColumn pos - 1) `mod` 4)) ts
         | t == "\r" || t == "\n" || t == "\r\n" ->
            Tok LineEnd pos t :
            go (incSourceLine (setSourceColumn pos 1) 1) ts
         | isAlphaNum thead ->
           Tok WordChars pos t :
           go (incSourceColumn pos (T.length t)) ts
         | isSpace thead ->
           Tok UnicodeSpace pos t :
           go (incSourceColumn pos (T.length t)) ts
         | T.length t == 1 ->
           Tok (Symbol thead) pos t :
           go (incSourceColumn pos 1) ts
         | otherwise -> error $ "Don't know what to do with" ++ show t
{-# SCC tokenize #-}

-- | Reverses 'tokenize'.  @untokenize . tokenize@ should be
-- the identity.
untokenize :: [Tok] -> Text
untokenize = mconcat . map tokContents
{-# SCC untokenize #-}
