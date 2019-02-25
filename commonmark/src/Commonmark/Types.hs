{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

module Commonmark.Types
  ( Format(..)
  , Html5(..)
  , RangedHtml5(..)
  , ListSpacing(..)
  , ListType(..)
  , IsInline(..)
  , IsBlock(..)
  , SourceRange(..)
  , SourcePos
  )
where
import           Data.Data            (Data)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.String          (IsString(..))
import           Data.Typeable        (Typeable)
import           Text.Parsec.Pos      (SourcePos, sourceColumn, sourceLine,
                                       sourceName)
import           Data.Semigroup       (Semigroup, (<>))
import           Data.Text.Lazy.Builder (Builder, fromText)
import qualified Data.Text.Lazy.Builder as Builder
import           Data.Char            (isSpace)
import           Commonmark.Html      (escapeHtmlChar, escapeHtml,
                                       escapeURI, innerText)
import           Commonmark.Entity    (lookupEntity)

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

newtype Html5 = Html5 {unHtml5 :: Builder}
  deriving (Show, Semigroup, Monoid, Typeable)

instance IsString Html5 where
  fromString = Html5 . Builder.fromString

newtype RangedHtml5 = RangedHtml5 {unRangedHtml5 :: Builder}
  deriving (Show, Semigroup, Monoid, Typeable)

instance IsString RangedHtml5 where
  fromString = RangedHtml5 . Builder.fromString

class (Monoid a, Show a) => IsInline a where
  lineBreak :: SourceRange -> a
  softBreak :: SourceRange -> a
  str :: SourceRange -> Text -> a
  entity :: SourceRange -> Text -> a
  escapedChar :: SourceRange -> Char -> a
  emph :: SourceRange -> a -> a
  strong :: SourceRange -> a -> a
  link :: SourceRange
       -> Text -- ^ Destination
       -> Text -- ^ Title
       -> a    -- ^ Link description
       -> a
  image :: SourceRange
        -> Text -- ^ Source
        -> Text -- ^ Title
        -> a    -- ^ Description
        -> a
  code :: SourceRange -> Text -> a
  rawInline :: SourceRange -> Format -> Text -> a

openTag :: SourceRange -> Text -> [(Text, Builder)] -> Html5
openTag sr tagname attr =
  Html5 $ "<" <> fromText tagname <> attribs sr attr <> ">"
{-# INLINE openTag #-}

selfClosingTag :: SourceRange -> Text -> [(Text, Builder)] -> Html5
selfClosingTag sr tagname attr =
  Html5 $ "<" <> fromText tagname <> attribs sr attr <> " />"
{-# INLINE selfClosingTag #-}

inTags :: SourceRange -> Text -> [(Text, Builder)] -> Html5 -> Html5
inTags sr tagname attr contents =
  openTag sr tagname attr <> contents <> closeTag tagname
{-# INLINE inTags #-}

htmlText :: SourceRange -> Builder -> Html5
htmlText (SourceRange []) txt = Html5 txt
htmlText sr txt = inTags sr "span" [] (Html5 txt)
{-# INLINE htmlText #-}

closeTag :: Text -> Html5
closeTag tagname =
  Html5 $ "</" <> fromText tagname <> ">"
{-# INLINE closeTag #-}

attribs :: SourceRange -> [(Text, Builder)] -> Builder
attribs (SourceRange []) [] = mempty
attribs sr attrs =
  let addSrAttrib = case sr of
                     SourceRange [] -> id
                     SourceRange r  -> (("data-sourcepos",
                                         fromText (T.pack (show r))):)
      renderAttrib (k,v) = " " <> fromText k <> "=\"" <> v <> "\""
  in  mconcat $ map renderAttrib $ addSrAttrib attrs
{-# INLINE attribs #-}

-- This instance mirrors what is expected in the spec tests.
instance IsInline Html5 where
  lineBreak sr = selfClosingTag sr "br" [] <> "\n"
  softBreak _sr = "\n"
  str sr t = htmlText sr (escapeHtml t)
  entity sr t = htmlText sr $
                     case lookupEntity (drop 1 $ T.unpack t) of
                          Just t' -> escapeHtml (T.pack t')
                          Nothing -> fromText t
  escapedChar sr c = htmlText sr (escapeHtmlChar c)
  emph sr ils = inTags sr "em" [] ils
  strong sr ils = inTags sr "strong" [] ils
  link sr target title ils = inTags sr "a"
    ([("href", escapeURI target)] <>
     [("title", escapeHtml title) | not (T.null title)]) ils
  image sr target title ils = selfClosingTag sr "img"
    ([("src", escapeURI target),
      ("alt", innerText (unHtml5 ils))] <>
     [("title", escapeHtml title) | not (T.null title)])
  code sr t = inTags sr "code" [] (Html5 (escapeHtml t))
  rawInline sr f t
    | f == Format "html" = htmlText sr (fromText t)
    | otherwise          = mempty

class (Monoid b, Show b, IsInline il)
      => IsBlock il b | b -> il where
  paragraph :: SourceRange -> il -> b
  plain :: SourceRange -> il -> b
  thematicBreak :: SourceRange -> b
  blockQuote :: SourceRange -> b -> b
  codeBlock :: SourceRange -> Text -> Text -> b
  header :: SourceRange
         -> Int -- ^ Level
         -> il  -- ^ text
         -> b
  rawBlock :: SourceRange -> Format -> Text -> b
  referenceLinkDefinition :: SourceRange
                          -> Text -- ^ Label
                          -> (Text, Text) -- ^ Destination, title
                          -> b
  list :: SourceRange -> ListType -> ListSpacing -> [b] -> b

instance IsBlock Html5 Html5 where
  paragraph sr ils = inTags sr "p" [] ils <> "\n"
  plain sr ils
    = case sr of
        SourceRange [] -> ils <> "\n"
        _ -> inTags sr "div" [] ils <> "\n"
  thematicBreak sr = selfClosingTag sr "hr" [] <> "\n"
  blockQuote sr bs = inTags sr "blockquote" [] bs <> "\n"
  codeBlock sr info t = inTags sr "pre" [] $
    inTags mempty "code" [("class", "language-" <> escapeHtml lang)
                           | not (T.null lang)]
      (Html5 (escapeHtml t)) <> "\n"
    where lang = T.takeWhile (not . isSpace) info
  header sr level ils = inTags sr h [] ils <> "\n"
    where h = case level of
                   1 -> "h1"
                   2 -> "h2"
                   3 -> "h3"
                   4 -> "h4"
                   5 -> "h5"
                   6 -> "h6"
                   _ -> "p"
  rawBlock sr f t
    | f == Format "html" =
      case sr of
        SourceRange [] -> Html5 $ fromText t
        _ -> inTags sr "div" [] (Html5 $ fromText t)
    | otherwise          = mempty
  referenceLinkDefinition _ _ _ = mempty
  list sr (BulletList _) lSpacing items =
    inTags sr "ul" [] ("\n" <> mconcat (map li items)) <> "\n"
   where li x = inTags mempty "li" []
                ((if lSpacing == TightList then mempty else "\n") <> x)
                <> "\n"
  list sr (OrderedList startnum _) lSpacing items =
    inTags sr "ol" attrs ("\n" <> mconcat (map li items)) <> "\n"
    where attrs = [("start", escapeHtml (fromString (show startnum)))
                     | startnum /= 1]
          li x = inTags mempty "li" []
                ((if lSpacing == TightList then mempty else "\n") <> x)
                <> "\n"

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
