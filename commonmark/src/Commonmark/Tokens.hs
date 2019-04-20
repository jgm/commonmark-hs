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
  , satisfy
  , char
  , anyChar
  , oneOf
  , noneOf
  , withRaw
  , charBehind
  , getOffset
  , gobbleSpaces
  , gobbleUpToSpaces
  ) where

import           Text.Parsec hiding (satisfy, char, anyChar, oneOf, noneOf)
import           Data.Data       (Data, Typeable)
import qualified Data.Vector.Unboxed as V
import           Data.List       (mapAccumL)

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
      let (!ln, !col) = subjectPositions subj V.! subjectOffset subj
      in  setSourceLine (setSourceColumn spos col) ln
{-# INLINEABLE tok #-}

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
{-# INLINEABLE withRaw #-}

charsFromOffsets :: V.Vector Char -> [(Offset, Offset)] -> [Char]
charsFromOffsets v = V.toList . mconcat . map charsFromOffset
  where charsFromOffset (start, end) = V.slice start (end - start) v

getOffset :: Monad m => ParsecT Subject u m Offset
getOffset = do
  subj <- getInput
  return $ subjectOffset subj
{-# INLINEABLE getOffset #-}

charBehind :: Monad m => ParsecT Subject u m (Maybe Char)
charBehind = do
  subj <- getInput
  let offset = subjectOffset subj
  case subjectRanges subj of
    (start,end):_
      | offset > start
      , offset <= end -> return $ subjectChars subj V.!? (offset - 1)
    _                 -> return Nothing

satisfy :: Monad m => (Char -> Bool) -> ParsecT Subject u m Char
satisfy f = tok (\c -> if f c then Just c else Nothing)
{-# INLINEABLE satisfy #-}

anyChar :: Monad m => ParsecT Subject u m Char
anyChar = tok Just
{-# INLINEABLE anyChar #-}

char :: Monad m => Char -> ParsecT Subject u m Char
char c = satisfy (== c)
{-# INLINEABLE char #-}

oneOf :: Monad m => [Char] -> ParsecT Subject u m Char
oneOf cs = satisfy (`elem` cs)
{-# INLINEABLE oneOf #-}

noneOf :: Monad m => [Char] -> ParsecT Subject u m Char
noneOf cs = satisfy (`notElem` cs)
{-# INLINEABLE noneOf #-}

-- | Parses exactly @n@ spaces. If tabs are encountered,
-- they are split into spaces before being consumed; so
-- a tab may be partially consumed by this parser.
gobbleSpaces :: (Monad m) => Int -> ParsecT Subject u m Int
gobbleSpaces 0 = return 0
gobbleSpaces n = try $ gobble' True n
{-# INLINEABLE gobbleSpaces #-}

-- | Parses up to @n@ spaces.
gobbleUpToSpaces :: (Monad m) => Int -> ParsecT Subject u m Int
gobbleUpToSpaces 0 = return 0
gobbleUpToSpaces n = gobble' False n
{-# INLINEABLE gobbleUpToSpaces #-}

gobble' :: (Monad m) => Bool -> Int -> ParsecT Subject u m Int
gobble' requireAll numspaces
  | numspaces >= 1 =
   try (satisfy (== ' ') >> (+1) <$> gobble' requireAll (numspaces - 1))
   <|>
   (do startpos <- getPosition
       endpos <- lookAhead $ satisfy (== '\t') *> getPosition
       case sourceColumn endpos - sourceColumn startpos of
         n | n < numspaces  -> do
               char '\t'
               (+ n) <$> gobble' requireAll (numspaces - n)
           | n == numspaces -> do
               char '\t'
               return n
           | otherwise      -> do
               -- leave \t on input stream
               setPosition $ incSourceColumn startpos numspaces
               return numspaces)
    <|> if requireAll
           then fail "not enough spaces to gobble"
           else return 0
  | otherwise     = return 0
{-# INLINEABLE gobble' #-}
