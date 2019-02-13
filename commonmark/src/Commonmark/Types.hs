{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Commonmark.Types
  ( Format(..)
  , ListSpacing(..)
  , ListType(..)
  , IsInline(..)
  , IsBlock(..)
  , SourceRange(..)
  , SourcePos(..)
  , Rangeable(..)
  )
where
import           Data.Data            (Data)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)
import           Data.Semigroup       (Semigroup, (<>))
import           Data.Text.Lazy.Builder (Builder, singleton, fromText,
                                         fromString)
import           Data.Char            (isSpace)
import           Commonmark.Html      (escapeHtmlChar, escapeHtml,
                                       escapeURI, innerText)
import           Commonmark.Entity    (lookupEntity)
import           Commonmark.ParserCombinators (SourcePos(..))

newtype Format = Format Text
  deriving (Show, Data, Typeable)

instance Eq Format where
  (Format t1) == (Format t2) = T.toCaseFold t1 == T.toCaseFold t2

data ListSpacing =
       TightList
     | LooseList
     deriving (Show, Ord, Eq, Data, Typeable)

data ListType =
       BulletList !Char
     | OrderedList !Int !Char
     deriving (Show, Ord, Eq, Data, Typeable)

class (Monoid a, Show a, Rangeable a) => IsInline a where
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

-- This instance mirrors what is expected in the spec tests.
instance IsInline Builder where
  lineBreak = "<br />\n"
  softBreak = "\n"
  str t = escapeHtml t
  entity t = case lookupEntity (drop 1 $ T.unpack t) of
                   Just t' -> escapeHtml (T.pack t')
                   Nothing -> fromText t
  escapedChar c = escapeHtmlChar c
  emph ils = "<em>" <> ils <> "</em>"
  strong ils = "<strong>" <> ils <> "</strong>"
  link target title ils = "<a href=\"" <> escapeURI target <> "\"" <>
    (if T.null title
        then mempty
        else " title=\"" <> escapeHtml title <> "\"") <>
    ">" <> ils <> "</a>"
  image target title ils = "<img src=\"" <>
    escapeURI target <> "\"" <>
    " alt=\"" <> innerText ils <> "\"" <>
    (if T.null title
        then mempty
        else " title=\"" <> escapeHtml title <> "\"") <>
    " />"
  code t = "<code>" <> escapeHtml t <> "</code>"
  rawInline f t
    | f == Format "html" = fromText t
    | otherwise          = mempty

class (Monoid b, Show b, Rangeable b, IsInline il)
      => IsBlock il b | b -> il where
  paragraph :: il -> b
  plain :: il -> b
  thematicBreak :: b
  blockQuote :: b -> b
  codeBlock :: Text -> Text -> b
  header :: Int -- ^ Level
         -> il  -- ^ text
         -> b
  rawBlock :: Format -> Text -> b
  referenceLinkDefinition :: Text -- ^ Label
                          -> (Text, Text) -- ^ Destination, title
                          -> b
  list :: ListType -> ListSpacing -> [b] -> b

instance IsBlock Builder Builder where
  paragraph ils = "<p>" <> ils <> "</p>" <> nl
  plain ils = ils <> nl
  thematicBreak = "<hr />" <> nl
  blockQuote bs = "<blockquote>" <> nl <> bs <>
    "</blockquote>" <> nl
  codeBlock info t = "<pre><code" <>
    (if T.null lang
        then ">"
        else fromText (" class=\"language-" <> lang <> "\">")) <>
    escapeHtml t <> "</code></pre>" <> nl
    where lang = T.takeWhile (not . isSpace) info
  header level ils = singleton '<' <> h <> singleton '>' <>
    ils <> "</" <> h <> singleton '>' <> nl
    where h = case level of
                   1 -> "h1"
                   2 -> "h2"
                   3 -> "h3"
                   4 -> "h4"
                   5 -> "h5"
                   6 -> "h6"
                   _ -> "p"
  rawBlock f t
    | f == Format "html" = fromText t
    | otherwise          = mempty
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) lSpacing items = "<ul>" <> nl <>
    mconcat (map li items) <> "</ul>" <> nl
   where li x = "<li>" <>
                (if lSpacing == TightList then mempty else nl) <> x <>
                "</li>" <> nl
  list (OrderedList startnum _) lSpacing items = "<ol" <>
    (if startnum /= 1
        then " start=\"" <> fromString (show startnum) <> "\""
        else mempty) <> ">" <> nl <>
    mconcat (map li items) <> "</ol>" <> nl
   where li x = "<li>" <>
                (if lSpacing == TightList then mempty else nl) <> x <>
                "</li>" <> nl

nl :: Builder
nl = "\n"

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

instance Rangeable Builder where
  ranged _ x = x

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
