{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MonoLocalBinds             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Commonmark.Types
  ( Format(..)
  , ListSpacing(..)
  , ListType(..)
  , DelimiterType(..)
  , EnumeratorType(..)
  , IsInline(..)
  , IsBlock(..)
  , SourceRange(..)
  , Rangeable(..)
  , Attribute
  , Attributes
  , HasAttributes(..)
  , ToPlainText(..)

  -- * Re-exports
  , module Text.Parsec.Pos
  )
where
import           Data.Data            (Data)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)
import           Text.Parsec.Pos      (SourcePos, sourceColumn, sourceLine,
                                       sourceName)

newtype Format = Format Text
  deriving (Show, Data, Typeable)

instance Eq Format where
  (Format t1) == (Format t2) = T.toCaseFold t1 == T.toCaseFold t2

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
     -- first Text is before, second Text is after enumerator
     deriving (Show, Ord, Eq, Data, Typeable)

class (Monoid a, Show a, Rangeable a, HasAttributes a) => IsInline a where
  lineBreak :: a
  softBreak :: a
  str :: Text -> a
  entity :: Text -> a
  escapedChar :: Char -> a
  emph :: a -> a
  strong :: a -> a
  link :: Text -- ^ Destination
       -> Text -- ^ Title
       -> a    -- ^ Link description
       -> a
  image :: Text -- ^ Source
        -> Text -- ^ Title
        -> a    -- ^ Description
        -> a
  code :: Text -> a
  rawInline :: Format -> Text -> a

instance {-# OVERLAPPABLE #-} (Applicative f, IsInline a, Monoid (f a), Show (f a)) => IsInline (f a) where
  lineBreak = pure lineBreak
  softBreak = pure softBreak
  str t = pure $ str t
  entity t = pure $ entity t
  escapedChar c = pure $ escapedChar c
  emph = fmap emph
  strong = fmap strong
  link d t = fmap $ link d t
  image s t = fmap $ image s t
  code t = pure $ code t
  rawInline f t = pure $ rawInline f t

class (Monoid b, Show b, Rangeable b, IsInline il, HasAttributes b)
      => IsBlock il b | b -> il where
  paragraph :: il -> b
  plain :: il -> b
  thematicBreak :: b
  blockQuote :: b -> b
  codeBlock :: Text -> Text -> b
  heading :: Int -- ^ Level
          -> il  -- ^ text
          -> b
  rawBlock :: Format -> Text -> b
  referenceLinkDefinition :: Text -- ^ Label
                          -> (Text, Text) -- ^ Destination, title
                          -> b
  list :: ListType -> ListSpacing -> [b] -> b

instance {-# OVERLAPPABLE #-} (Applicative f, Monoid (f il), Show (f il), Monoid (f b), Show (f b), IsBlock il b) => IsBlock (f il) (f b) where
  paragraph = fmap paragraph
  plain = fmap plain
  thematicBreak = pure thematicBreak
  blockQuote = fmap blockQuote
  codeBlock p q = pure $ codeBlock p q
  heading l = fmap $ heading l
  rawBlock f t = pure $ rawBlock f t
  referenceLinkDefinition l dt = pure $ referenceLinkDefinition l dt
  list lt ls fbs = fmap (list lt ls) $ sequenceA fbs

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

instance {-# OVERLAPPABLE #-} (Functor f, Rangeable a) => Rangeable (f a) where
  ranged sr = fmap $ ranged sr

prettyRange :: SourceRange -> String
prettyRange (SourceRange xs) = go "" xs
  where
    go _ [] = ""
    go curname ((p1,p2):rest)
      = (if sourceName p1 /= curname
            then sourceName p1 ++ "@"
            else "") ++
         show (sourceLine p1) ++ ":" ++
         show (sourceColumn p1) ++ "-" ++
         (if sourceName p2 /= sourceName p1
             then sourceName p2 ++ "@"
             else "") ++
         show (sourceLine p2) ++
         ":" ++ show (sourceColumn p2) ++
         if null rest
            then ""
            else ";" ++ go (sourceName p2) rest

type Attribute = (Text, Text)

type Attributes = [Attribute]

class HasAttributes a where
  addAttributes :: Attributes -> a -> a

instance {-# OVERLAPPABLE #-} (Functor f, HasAttributes a) => HasAttributes (f a) where
  addAttributes attrs = fmap $ addAttributes attrs

class ToPlainText a where
  toPlainText :: a -> Text
