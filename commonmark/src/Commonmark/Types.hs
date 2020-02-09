{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MonoLocalBinds             #-}

module Commonmark.Types
  ( Format(..)
  , ListSpacing(..)
  , ListType(..)
  , DelimiterType(..)
  , EnumeratorType(..)
  , IsInline(..)
  , IsBlock(..)
  , SourceRange(..)
  , SourcePos
  , Rangeable(..)
  , Attribute
  , Attributes
  , HasAttributes(..)
  , ToPlainText(..)
  )
where
import           Data.Data            (Data)
import           Data.ByteString      (ByteString)
import           Data.Typeable        (Typeable)
import           Text.Parsec.Pos      (SourcePos, sourceColumn, sourceLine,
                                       sourceName)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup       (Semigroup, (<>))
#endif

newtype Format = Format ByteString
  deriving (Show, Data, Typeable)

instance Eq Format where
  (Format t1) == (Format t2) = T.toCaseFold (TE.decodeUtf8 t1) ==
                               T.toCaseFold (TE.decodeUtf8 t2)

data ListSpacing =
       TightList
     | LooseList
     deriving (Show, Ord, Eq, Data, Typeable)

data EnumeratorType =
       Decimal
     | UpperAlpha
     | LowerAlpha
     | UpperRoman
     | LowerRoman
     deriving (Show, Ord, Eq, Data, Typeable)

data DelimiterType =
       Period
     | OneParen
     | TwoParens
     deriving (Show, Ord, Eq, Data, Typeable)

data ListType =
       BulletList !Char
     | OrderedList !Int !EnumeratorType !DelimiterType
     deriving (Show, Ord, Eq, Data, Typeable)

class (Monoid a, Show a, Rangeable a, HasAttributes a) => IsInline a where
  lineBreak :: a
  softBreak :: a
  str :: ByteString -> a
  entity :: ByteString -> a
  escapedChar :: Char -> a
  emph :: a -> a
  strong :: a -> a
  link :: ByteString -- ^ Destination
       -> ByteString -- ^ Title
       -> a    -- ^ Link description
       -> a
  image :: ByteString -- ^ Source
        -> ByteString -- ^ Title
        -> a    -- ^ Description
        -> a
  code :: ByteString -> a
  rawInline :: Format -> ByteString -> a

class (Monoid b, Show b, Rangeable b, IsInline il, HasAttributes b)
      => IsBlock il b | b -> il where
  paragraph :: il -> b
  plain :: il -> b
  thematicBreak :: b
  blockQuote :: b -> b
  codeBlock :: ByteString -> ByteString -> b
  heading :: Int -- ^ Level
          -> il  -- ^ text
          -> b
  rawBlock :: Format -> ByteString -> b
  referenceLinkDefinition :: ByteString -- ^ Label
                          -> (ByteString, ByteString)
                                        -- ^ Destination, title
                          -> b
  list :: ListType -> ListSpacing -> [b] -> b

newtype SourceRange = SourceRange
        { unSourceRange :: [(SourcePos, SourcePos)] }
  deriving (Eq, Ord, Data, Typeable)

instance Semigroup SourceRange where
  (SourceRange xs) <> (SourceRange ys) =
    SourceRange (consolidateRanges xs ys)

instance Monoid SourceRange where
  mempty = SourceRange mempty
  mappend = (<>)

consolidateRanges :: Eq a => [(a,a)] -> [(a,a)] -> [(a,a)]
consolidateRanges [] xs = xs
consolidateRanges xs [] = xs
consolidateRanges xs@(_:_) ((s2,e2):ys) =
  if e1 == s2
     then init xs ++ (s1,e2):ys
     else xs ++ (s2,e2):ys
  where (s1,e1) = last xs

instance Show SourceRange where
  show = prettyRange

class Rangeable a where
  ranged :: SourceRange -> a -> a

prettyRange :: SourceRange -> String
prettyRange (SourceRange []) = ""
prettyRange (SourceRange xs@((p,_):_)) =
  sourceName p ++ "@" ++ go (sourceName p) xs
  where
    go _ [] = ""
    go curname ((p1,p2):rest)
      | sourceName p1 /= curname =
         sourceName p1 ++ "@" ++ go (sourceName p) ((p1,p2):rest)
      | otherwise =
         show (sourceLine p1) ++ ":" ++
         show (sourceColumn p1) ++ "-" ++
         (if sourceName p2 /= curname
             then sourceName p2 ++ "@"
             else "") ++ show (sourceLine p2) ++
         ":" ++ show (sourceColumn p2) ++
         if null rest
            then ""
            else ";" ++ go (sourceName p2) rest

type Attribute = (ByteString, ByteString)

type Attributes = [Attribute]

class HasAttributes a where
  addAttributes :: Attributes -> a -> a

class ToPlainText a where
  toPlainText :: a -> ByteString
