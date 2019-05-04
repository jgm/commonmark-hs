{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MonoLocalBinds             #-}

module Commonmark.Types
  ( Options(..)
  , defaultOptions
  , Format(..)
  , ListSpacing(..)
  , ListType(..)
  , IsInline(..)
  , IsBlock(..)
  , SourceRange(..)
  , SourcePos
  , Rangeable(..)
  , Html
  , renderHtml
  )
where
import           Data.Data            (Data)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Typeable        (Typeable)
import           Text.Parsec.Pos      (SourcePos, sourceColumn, sourceLine,
                                       sourceName)
import           Data.Semigroup       (Semigroup, (<>))
import           Data.Char            (isSpace)
import           Commonmark.Html      (Html, escapeURI, innerText,
                                       renderHtml,
                                       htmlInline, htmlBlock, addAttribute,
                                       htmlText, htmlRaw)
import           Commonmark.Entity    (lookupEntity)

data Options =
  Options
  { sourcePositions :: Bool
  } deriving (Show, Data, Typeable)

defaultOptions :: Options
defaultOptions =
  Options { sourcePositions = False }

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
instance IsInline Html where
  lineBreak = htmlInline "br" Nothing <> nl
  softBreak = nl
  str t = htmlText t
  entity t = case lookupEntity (drop 1 $ T.unpack t) of
                   Just t' -> htmlText (T.pack t')
                   Nothing -> htmlRaw t
  escapedChar c = htmlText (T.singleton c)
  emph ils = htmlInline "em" (Just ils)
  strong ils = htmlInline "strong" (Just ils)
  link target title ils =
    addAttribute ("href", escapeURI target) .
    (if T.null title
        then id
        else addAttribute ("title", title)) $
    htmlInline "a" (Just ils)
  image target title ils =
    addAttribute ("src", escapeURI target) .
    addAttribute ("alt", innerText ils) .
    (if T.null title
        then id
        else addAttribute ("title", title)) $
    htmlInline "img" Nothing
  code t = htmlInline "code" (Just (htmlText t))
  rawInline f t
    | f == Format "html" = htmlRaw t
    | otherwise          = mempty

class (Monoid b, Show b, Rangeable b, IsInline il)
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

instance IsBlock Html Html where
  paragraph ils = htmlBlock "p" (Just ils)
  plain ils = ils <> nl
  thematicBreak = htmlBlock "hr" Nothing
  blockQuote bs = htmlBlock "blockquote" $ Just (nl <> bs)
  codeBlock info t =
    htmlBlock "pre" $ Just $
    (if T.null lang
        then id
        else addAttribute ("class", "language-" <> lang)) $
    htmlInline "code" $ Just (htmlText t)
    where lang = T.takeWhile (not . isSpace) info
  heading level ils = htmlBlock h (Just ils)
    where h = case level of
                   1 -> "h1"
                   2 -> "h2"
                   3 -> "h3"
                   4 -> "h4"
                   5 -> "h5"
                   6 -> "h6"
                   _ -> "p"
  rawBlock f t
    | f == Format "html" = htmlRaw t
    | otherwise          = mempty
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) lSpacing items =
    htmlBlock "ul" $ Just (nl <> mconcat (map li items))
   where li x = htmlBlock "li" $
                   Just ((if lSpacing == TightList
                             then mempty
                             else nl) <> x)
  list (OrderedList startnum _) lSpacing items =
    (if startnum /= 1
        then addAttribute ("start", T.pack (show startnum))
        else id) $
    htmlBlock "ol" $
      Just (nl <> mconcat (map li items))
   where li x = htmlBlock "li" $
                   Just ((if lSpacing == TightList
                             then mempty
                             else nl) <> x)

nl :: Html
nl = htmlRaw "\n"

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

instance Rangeable Html where
  ranged sr x = addAttribute ("data-sourcepos", T.pack (prettyRange sr)) x

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
