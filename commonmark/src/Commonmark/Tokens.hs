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
  , withRaw
  , charBehind
  ) where

import           Text.Parsec
import           Data.Data       (Data, Typeable)
import qualified Data.Vector.Unboxed as V
import           Data.List       (mapAccumL)
import           Control.Monad   (guard, mzero)

type Offset = Int

data Subject =
  Subject
  { subjectRanges    :: [(Offset,Offset)] -- start, end
  , subjectOffset    :: Offset  -- current
  , subjectChars     :: V.Vector Char
  , subjectPositions :: V.Vector (Int, Int)
  } deriving (Data, Typeable, Show)

toSubject :: String -> Subject
toSubject s =
  Subject
  { subjectRanges   = [(0, V.length chars)]
  , subjectOffset   = 0
  , subjectChars     = chars
  , subjectPositions = V.fromList $ snd $ mapAccumL f (1,1) s }
 where
    chars = V.fromList s
    f :: (Int, Int) -> Char -> ((Int, Int), (Int, Int))
    f !spos !c = (adjustPos spos c, spos)
    adjustPos :: (Int, Int) -> Char -> (Int, Int)
    adjustPos (!ln, _)    '\n' = (ln + 1, 1)
    adjustPos (!ln, !col) '\t' = (ln, col + (4 - ((col - 1) `mod` 4)))
    adjustPos (!ln, !col) _    = (ln, col + 1)

instance Monad m => Stream Subject m Char where
  uncons subj@(Subject
         { subjectRanges   = ranges
         , subjectOffset   = offset
         , subjectChars    = chars
         }) =
    case ranges of
      []               -> return Nothing
      (cur,end):rest
        | offset < cur || offset >= end -> return Nothing
        | otherwise ->
            case chars V.!? offset of
              Just c  -> return $ Just
                (c, if offset + 1 >= end
                       then subj{ subjectRanges = rest
                                , subjectOffset =
                                    case rest of
                                      (cur',_):_ -> cur'
                                      _          -> offset }
                       else subj{ subjectOffset = offset + 1 } )
              Nothing -> return Nothing

tok :: Monad m => (Char -> Maybe a) -> ParsecT Subject u m a
tok = tokenPrim pprint updatePos
  where
    pprint :: Char -> String
    pprint c = [c]
    updatePos :: SourcePos -> Char -> Subject -> SourcePos
    updatePos spos _ subj =
      case subjectRanges subj of
             [] -> spos
             (cur,_):_ ->
                let (!ln, !col) = subjectPositions subj V.! cur
                in  setSourceLine (setSourceColumn spos col) ln

withRaw :: Monad m => ParsecT Subject u m a -> ParsecT Subject u m (a, [Char])
withRaw p = do
  subj <- getInput
  res <- p
  subj' <- getInput
  let prefix = take
         (length (subjectRanges subj) - length (subjectRanges subj'))
         (subjectRanges subj)
  let chars = charsFromOffsets (subjectChars subj) $
       prefix ++
       case subjectRanges subj' of
             [] -> []
             (start,_):_ -> [(start, subjectOffset subj')]
  return (res, chars)

charsFromOffsets :: V.Vector Char -> [(Offset, Offset)] -> [Char]
charsFromOffsets v = V.toList . mconcat . map charsFromOffset
  where charsFromOffset (start, end) = V.slice start (end - start) v

getOffset :: Monad m => ParsecT Subject u m Offset
getOffset = do
  subj <- getInput
  return $ subjectOffset subj

charBehind :: Monad m => ParsecT Subject u m (Maybe Char)
charBehind = do
  subj <- getInput
  let offset = subjectOffset subj
  case subjectRanges subj of
    (start,end):_
      | offset > start
      , offset <= end -> return $ subjectChars subj V.!? (offset - 1)
    _                 -> return Nothing
