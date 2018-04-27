{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Commonmark.Types
  ( Format(..)
  , ListSpacing(..)
  , ListType(..)
  , IsInline(..)
  , IsBlock(..)
  , SourceRange(..)
  , SourcePos
  , Rangeable(..)
  , RangedHtml(..)
  )
where
import           Data.Char            (isSpace)
import           Data.Data            (Data)
import           Data.Semigroup       (Semigroup, (<>))
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Read       as TR
import           Data.Typeable        (Typeable)
import           Network.URI          (escapeURIString, isAllowedInURI)
import           Text.HTML.TagSoup    (Tag (..), fromAttrib, parseTags)
import           Text.Parsec.Pos      (SourcePos, sourceColumn, sourceLine,
                                       sourceName)
import           Data.CaseInsensitive (mk)


newtype Format = Format Text
  deriving (Show, Data, Typeable)

instance Eq Format where
  (Format t1) == (Format t2) = mk t1 == mk t2

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

{-
instance IsInline (Html ()) where
  lineBreak = br_ [] <> "\n"
  softBreak = "\n"
  str t = toHtml t
  entity t
    | illegalCodePoint t = toHtml ("\xFFFD" :: Text)
    | otherwise = toHtmlRaw t
  escapedChar c = toHtml (T.singleton c)
  emph ils = em_ ils
  strong ils = strong_ ils
  link target title ils = a_ (href_ (escapeURI target) :
                              [title_ title | not (T.null title)])
                              ils
  image target title ils = img_ ([src_ (escapeURI target),
                                  alt_ (renderAlt ils)] ++
                                 [title_ title | not (T.null title)])
  code t = code_ (toHtml t)
  rawInline f t
    | f == Format "html" = toHtmlRaw t
    | otherwise          = mempty

escapeURI :: Text -> Text
escapeURI = T.pack . escapeURIString
  (\c -> isAllowedInURI c && c /= '[' && c /= ']') . T.unpack

renderAlt :: Html () -> Text
renderAlt = mconcat . map textOrAlt . parseTags . TL.toStrict . renderText
  where textOrAlt (TagText t)           = t
        textOrAlt tag@(TagOpen "img" _) = fromAttrib "alt" tag
        textOrAlt _                     = mempty

illegalCodePoint :: Text -> Bool
illegalCodePoint t =
  "&#" `T.isPrefixOf` t &&
  let t' = T.drop 2 $ T.filter (/=';') t
      badvalue (n, r) = not (T.null r) ||
                        n < 1 ||
                        n > (0x10FFFF :: Integer)
  in
  case T.uncons t' of
       Nothing -> True
       Just (x, rest)
         | x == 'x' || x == 'X'
           -> either (const True) badvalue (TR.hexadecimal rest)
         | otherwise
           -> either (const True) badvalue (TR.decimal t')
-}

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

{-
instance IsBlock (Html ()) (Html ()) where
  paragraph ils = p_ ils <> nl
  plain ils = ils <> nl
  thematicBreak = hr_ [] <> nl
  blockQuote bs = blockquote_ (nl <> bs) <> nl
  codeBlock info t = pre_ (with code_ attr (toHtml t)) <> nl
    where attr = [class_ ("language-" <> lang) | not (T.null info)]
          lang = T.takeWhile (not . isSpace) info
  header level ils = h ils <> nl
    where h = case level of
                   1 -> h1_
                   2 -> h2_
                   3 -> h3_
                   4 -> h4_
                   5 -> h5_
                   6 -> h6_
                   _ -> p_
  rawBlock f t
    | f == Format "html" = toHtmlRaw t
    | otherwise          = mempty
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) lSpacing items = ul_
    (nl <> mconcat (map li items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                   else li_ (nl <> x) <> nl
  list (OrderedList startnum _) lSpacing items = with ol_ attr
    (nl <> mconcat (map li items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                    else li_ (nl <> x) <> nl
         attr = [start_ (T.pack (show startnum)) | startnum /= 1]

nl :: Html ()
nl = toHtmlRaw ("\n" :: Text)

newtype RangedHtml = RangedHtml {unRangedHtml :: Html ()}
  deriving (Show, Semigroup, Monoid)

instance IsInline RangedHtml where
  lineBreak = RangedHtml $ br_ [] <> "\n"
  softBreak = RangedHtml "\n"
  str t = RangedHtml $ span_ $ toHtml t
  entity t
    | illegalCodePoint t = RangedHtml $ span_ $ toHtml ("\xFFFD" :: Text)
    | otherwise = RangedHtml $ span_ $ toHtmlRaw t
  escapedChar c = RangedHtml $ span_ $ toHtml (T.singleton c)
  emph ils = RangedHtml $ em_ $ unRangedHtml ils
  strong ils = RangedHtml $ strong_ $ unRangedHtml ils
  link target title ils = RangedHtml $
    a_ (href_ (escapeURI target) : [title_ title | not (T.null title)])
    $ unRangedHtml ils
  image target title ils = RangedHtml $
    img_ ([src_ (escapeURI target), alt_ (renderAlt $ unRangedHtml ils)] ++
                                   [title_ title | not (T.null title)])
  code t = RangedHtml $ code_ (toHtml t)
  rawInline f t
    | f == Format "html" = RangedHtml $ span_ $ toHtmlRaw t
    | otherwise          = mempty

instance IsBlock RangedHtml RangedHtml where
  paragraph ils = RangedHtml $ p_ (unRangedHtml ils) <> nl
  plain ils = RangedHtml $ unRangedHtml ils <> nl
  thematicBreak = RangedHtml $ hr_ [] <> nl
  blockQuote bs = RangedHtml $ blockquote_ (nl <> unRangedHtml bs) <> nl
  codeBlock info t = RangedHtml $ pre_ (with code_ attr (toHtml t)) <> nl
    where attr = [class_ ("language-" <> lang) | not (T.null info)]
          lang = T.takeWhile (not . isSpace) info
  header level ils = RangedHtml $ h (unRangedHtml ils) <> nl
    where h = case level of
                   1 -> h1_
                   2 -> h2_
                   3 -> h3_
                   4 -> h4_
                   5 -> h5_
                   6 -> h6_
                   _ -> p_
  rawBlock f t
    | f == Format "html" = RangedHtml $ toHtmlRaw t
    | otherwise          = mempty
  referenceLinkDefinition _ _ = mempty
  list (BulletList _) lSpacing items = RangedHtml $ ul_
    (nl <> mconcat (map (li . unRangedHtml) items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                   else li_ (nl <> x) <> nl
  list (OrderedList startnum _) lSpacing items = RangedHtml $ with ol_ attr
    (nl <> mconcat (map (li . unRangedHtml) items)) <> nl
   where li x = if lSpacing == TightList
                   then li_ x <> nl
                    else li_ (nl <> x) <> nl
         attr = [start_ (T.pack (show startnum)) | startnum /= 1]

instance Rangeable RangedHtml where
  ranged r (RangedHtml x) =
    RangedHtml $ with x [data_ "sourcepos" (T.pack (show r))]

instance Rangeable (Html ()) where
  ranged _ x = x
-}

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
