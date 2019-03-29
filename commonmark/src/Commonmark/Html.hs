{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Html
  ( Html
  , HtmlAttribute
  , htmlInline
  , htmlBlock
  , htmlText
  , htmlRaw
  , addAttribute
  , renderHtml
  , escapeURI
  , escapeHtml
  , escapeHtmlChar
  , innerText
  )
where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, fromText, toLazyText)
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup       ((<>))
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii)
import           Data.Maybe           (fromMaybe)

data ElementType =
    InlineElement
  | BlockElement

data Html a =
    HtmlElement ElementType Text [HtmlAttribute] (Maybe (Html a))
  | HtmlText Text
  | HtmlRaw Text
  | HtmlNull
  | HtmlConcat (Html a) (Html a)

instance Show (Html a) where
  show = TL.unpack . renderHtml

type HtmlAttribute = (Text, Text)

instance Semigroup (Html a) where
  x <> HtmlNull = x
  HtmlNull <> x = x
  x <> y        = HtmlConcat x y

instance Monoid (Html a) where
  mempty = HtmlNull
  mappend = (<>)

htmlInline :: Text -> Maybe (Html a) -> Html a
htmlInline tagname mbcontents = HtmlElement InlineElement tagname [] mbcontents

htmlBlock :: Text -> Maybe (Html a) -> Html a
htmlBlock tagname mbcontents = HtmlElement BlockElement tagname [] mbcontents

htmlText :: Text -> Html a
htmlText = HtmlText

htmlRaw :: Text -> Html a
htmlRaw = HtmlRaw

addAttribute :: HtmlAttribute -> Html a -> Html a
addAttribute attr (HtmlElement eltType tagname attrs mbcontents) =
  HtmlElement eltType tagname (attr:attrs) mbcontents
addAttribute _ elt = elt

renderHtml :: Html a -> TL.Text
renderHtml = toLazyText . toBuilder

toBuilder :: Html a -> Builder
toBuilder (HtmlNull) = mempty
toBuilder (HtmlConcat x y) = toBuilder x <> toBuilder y
toBuilder (HtmlRaw t) = fromText t
toBuilder (HtmlText t) = fromText (escapeHtml t)
toBuilder (HtmlElement eltType tagname attrs mbcontents) =
  "<" <> fromText tagname <> mconcat (map toAttr attrs) <> filling <> nl
  where
    toAttr (x,y) = " " <> fromText x <> "=\"" <> fromText (escapeHtml y) <> "\""
    nl = case eltType of
           BlockElement -> "\n"
           _            -> mempty
    filling = case mbcontents of
                 Nothing   -> " />"
                 Just cont -> ">" <> toBuilder cont <> "</" <>
                              fromText tagname <> ">"

escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeHtmlChar

escapeHtmlChar :: Char -> Text
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = T.singleton c

escapeURI :: Text -> Text
escapeURI = mconcat . map escapeURIChar . B.unpack . encodeUtf8

escapeURIChar :: Char -> Text
escapeURIChar c
  | isEscapable c = T.singleton '%' <> T.pack (printf "%02X" (ord c))
  | otherwise     = T.singleton c
  where isEscapable d = not (isAscii d && isAlphaNum d)
                     && d `notElem` ['%','/','?',':','@','-','.','_','~','&',
                                     '#','!','$','\'','(',')','*','+',',',
                                     ';','=']

innerText :: Html a -> Text
innerText (HtmlElement InlineElement "img" attrs Nothing) =
  fromMaybe mempty $ lookup "alt" attrs
innerText (HtmlElement _ _ _ (Just x)) = innerText x
innerText (HtmlText t)     = t
innerText (HtmlConcat x y) = innerText x <> innerText y
innerText _                = mempty
