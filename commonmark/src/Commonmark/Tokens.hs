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
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as UTF8
import           Data.Data       (Data, Typeable)
import           Text.Parsec.Pos

data Tok = Tok { tokType     :: !TokType
               , tokPos      :: !SourcePos
               , tokContents :: !ByteString
               }
               deriving (Show, Eq, Data, Typeable)

data TokType =
       Spaces
     | UnicodeSpace
     | LineEnd
     | WordChars
     | Symbol !Char
     deriving (Show, Eq, Ord, Data, Typeable)

-- | Convert a 'ByteString' (assumed UTF-8 encoded)
-- into a list of 'Tok'. The first parameter is the source name.
tokenize :: String -> ByteString -> [Tok]
tokenize name = go (initialPos name)
  where
   go :: SourcePos -> ByteString -> [Tok]
   go !pos !bs =
       case UTF8.decode bs of
         Nothing -> []
         Just (c, len)

          | c == '\r' ->
            let rest = B.drop 1 bs in
            case UTF8.decode rest of
              Just ('\n', _) ->
                Tok LineEnd pos (B.take 2 bs) :
                go (incSourceLine (setSourceColumn pos 1) 1) (B.drop 2 bs)
              _ -> Tok LineEnd pos (B.take 1 bs) :
                   go (incSourceLine (setSourceColumn pos 1) 1) rest

          | c == '\n' ->
                Tok LineEnd pos (B.take len bs) :
                go (incSourceLine (setSourceColumn pos 1) 1) (B.drop len bs)

          | c == ' ' ->
             let (sps, rest) = B.span (== 20) bs
              in Tok Spaces pos sps :
                 go (incSourceColumn pos (B.length sps)) rest

          | c == '\t' ->
             Tok Spaces pos (B.take len bs) :
             go (incSourceColumn pos (4 - (sourceColumn pos - 1) `mod` 4))
                 (B.drop len bs)

          | isSpace c ->
             Tok UnicodeSpace pos (B.take len bs) :
             go (incSourceColumn pos 1) (B.drop len bs)

          | isAlphaNum c ->
             let (cs, rest) = UTF8.span isAlphaNum bs
              in Tok WordChars pos cs :
                 go (incSourceColumn pos (UTF8.length cs)) rest

          | otherwise ->
             Tok (Symbol c) pos (B.take len bs) :
             go (incSourceColumn pos 1) (B.drop len bs)
{-# SCC tokenize #-}

-- | Reverses 'tokenize'.  @untokenize . tokenize@ should be
-- the identity.
untokenize :: [Tok] -> ByteString
untokenize = mconcat . map tokContents
{-# SCC untokenize #-}
