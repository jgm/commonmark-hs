{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Commonmark.Tokens
  ( Tok(..)
  , TokType(..)
  , Offset
  , tokenize
  , untokenize
  , tokToString
  ) where

import           Data.Char       (isAlphaNum, isSpace)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           Data.Data       (Data, Typeable)
import qualified Data.Vector.Unboxed as V

type Offset = Int

data Tok = Tok { tokType     :: !TokType
               , tokOffset   :: !Offset
               , tokLength   :: !Int
               , tokSubject  :: V.Vector Char
               } deriving (Eq, Data, Typeable)

instance Show Tok where
  show (Tok tt pos len subj) =
    show tt ++ " " ++ show pos ++ " " ++ show (V.unsafeSlice pos len subj)


data TokType =
       Spaces
     | Tab
     | UnicodeSpace
     | LineEnd
     | WordChars
     | Symbol !Char
     deriving (Show, Eq, Ord, Data, Typeable)

-- | Convert a 'Text' into a list of 'Tok'. The first parameter
-- species the source name.
tokenize :: Text -> [Tok]
tokenize t = go 0
  where
    v = V.fromList (T.unpack t)
    go pos =
      case v V.!? pos of
        Nothing  -> []
        Just !c  ->
          let (!tok, !newpos) = getTok c pos
          in  (tok : go newpos)
    getTok c pos =
      case c of
        ' ' -> case V.findIndex (/=' ') (V.drop (pos + 1) v) of
                 Just firstNonspace ->
                   (Tok Spaces pos (1 + firstNonspace) v,
                    pos + 1 + firstNonspace)
                 Nothing ->
                   (Tok Spaces pos (V.length v - pos) v,
                    V.length v)
        '\t' -> (Tok Tab pos 1 v, pos + 1)
        '\n' -> (Tok LineEnd pos 1 v, pos + 1)
        '\r' -> case v V.!? (pos + 1) of
                   Just '\n' ->
                     (Tok LineEnd pos 2 v, pos + 2)
                   _         ->
                     (Tok LineEnd pos 1 v, pos + 1)
        _ | isAlphaNum c ->
               case V.findIndex (not . isAlphaNum) (V.drop (pos + 1) v) of
                 Just firstNonAlphaNum ->
                   (Tok WordChars pos (1 + firstNonAlphaNum) v,
                    pos + 1 + firstNonAlphaNum)
                 Nothing ->
                   (Tok WordChars pos (V.length v - pos) v,
                    V.length v)
          | isSpace c -> (Tok UnicodeSpace pos 1 v, pos + 1)
          | otherwise -> (Tok (Symbol c) pos 1 v, pos + 1)

-- | Reverses 'tokenize'.  @untokenize . tokenize ""@ should be
-- the identity.
untokenize :: [Tok] -> Text
untokenize = mconcat . map (T.pack . tokToString)

-- | Render token contents as a String.
tokToString :: Tok -> String
tokToString (Tok Spaces pos len subj) =
      case V.toList $ V.unsafeSlice pos len subj of
        "\t"  -> replicate len ' '
        s     -> s
tokToString (Tok _ pos len subj) = V.toList $ V.unsafeSlice pos len subj
