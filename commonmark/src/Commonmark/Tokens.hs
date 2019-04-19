{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UnboxedTuples #-}

module Commonmark.Tokens
  ( Subject(..)
  , Offset
  , SourcePos
  , toSubject
  , tok
  ) where

import           Text.Parsec
import           Data.Data       (Data, Typeable)
import qualified Data.Vector.Unboxed as V
import           Data.List       (mapAccumL)
type Offset = Int

data Subject =
  Subject
  { subjectOffset    :: Offset
  , subjectChars     :: V.Vector Char
  , subjectPositions :: V.Vector (Int, Int)
  } deriving (Data, Typeable, Show)

toSubject :: String -> Subject
toSubject s =
  Subject
  { subjectOffset    = 0
  , subjectChars     = V.fromList s
  , subjectPositions = V.fromList $ snd $ mapAccumL f (1,1) s }
 where
    f :: (Int, Int) -> Char -> ((Int, Int), (Int, Int))
    f !spos !c = (adjustPos spos c, spos)
    adjustPos :: (Int, Int) -> Char -> (Int, Int)
    adjustPos (!ln, _)    '\n' = (ln + 1, 1)
    adjustPos (!ln, !col) '\t' = (ln, col + (4 - ((col - 1) `mod` 4)))
    adjustPos (!ln, !col) _    = (ln, col + 1)

instance Monad m => Stream Subject m Char where
  uncons subj =
    case subjectChars subj V.!? subjectOffset subj of
      Just c  -> return $ Just (c, subj{ subjectOffset =
                                       subjectOffset subj + 1 })
      Nothing -> return Nothing

tok :: Monad m => (Char -> Maybe a) -> ParsecT Subject u m a
tok = tokenPrim pprint updatePos
  where
    pprint :: Char -> String
    pprint c = [c]
    updatePos :: SourcePos -> Char -> Subject -> SourcePos
    updatePos spos _ subj =
      let (!ln, !col) = subjectPositions subj V.! subjectOffset subj
      in  setSourceLine (setSourceColumn spos col) ln
