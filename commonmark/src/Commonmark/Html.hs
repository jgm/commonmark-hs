{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Html
  ( ElementType(..)
  , Html
  , HtmlAttribute
  , htmlElement
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
import           Data.Text.Lazy.Builder (Builder, singleton, fromText,
                                         toLazyText, fromString,
                                         fromLazyText)
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup       ((<>))
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii)
import           Text.Parsec
import           Commonmark.Util      (skipManyTill)

data ElementType =
    InlineElement
  | BlockElement

data Html a =
    HtmlElement ElementType Text [HtmlAttribute] (Maybe (Html a))
  | HtmlText Text
  | HtmlRaw Text
  | HtmlNull
  | HtmlConcat (Html a) (Html a)

type HtmlAttribute = (Text, Text)

instance Semigroup (Html a) where
  x <> HtmlNull = x
  HtmlNull <> x = x
  x <> y        = HtmlConcat x y

instance Monoid (Html a) where
  mempty = HtmlNull
  mappend = (<>)

htmlElement :: ElementType -> Text -> [(Text, Text)] -> Maybe (Html a) -> Html a
htmlElement eltType tagname attrs mbcontents =
  HtmlElement eltType tagname attrs mbcontents

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
toBuilder (HtmlText t) = escapeHtml t
toBuilder (HtmlElement eltType tagname attrs mbcontents) =
  "<" <> fromText tagname <> mconcat (map toAttr attrs) <> filling <> nl
  where
    toAttr (x,y) = " " <> fromText x <> "=\"" <> escapeHtml y <> "\""
    nl = case eltType of
           BlockElement -> "\n"
           _            -> mempty
    filling = case mbcontents of
                 Nothing   -> " />"
                 Just cont -> ">" <> toBuilder cont <> "</" <>
                              fromText tagname <> ">"

escapeHtml :: Text -> Builder
escapeHtml = foldMap escapeHtmlChar . T.unpack

escapeHtmlChar :: Char -> Builder
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = singleton c

escapeURI :: Text -> Builder
escapeURI = foldMap escapeURIChar . B.unpack .  encodeUtf8

escapeURIChar :: Char -> Builder
escapeURIChar c
  | c == '&'      = "&amp;"
  | isEscapable c = singleton '%' <> fromString (printf "%02X" (ord c))
  | otherwise     = escapeHtmlChar c
  where isEscapable d = not (isAscii d && isAlphaNum d)
                     && d `notElem` ['%','/','?',':','@','-','.','_','~','&',
                                     '#','!','$','\'','(',')','*','+',',',
                                     ';','=']

innerText :: Builder -> Builder
innerText = getInnerText . toLazyText

pInnerText :: Parsec TL.Text () Builder
pInnerText = (mconcat <$> many (pTag <|> pNonTag)) <* eof

pTag :: Parsec TL.Text () Builder
pTag = do
  char '<'
  cs <- many (satisfy isAlphaNum)
  if cs == "img" || cs == "IMG"
     then do
       alt <- option "" $ try $ do
                skipManyTill anyChar (try (string "alt=\""))
                manyTill anyChar (char '"')
       skipManyTill anyChar (char '>')
       return $ fromString alt
     else mempty <$ skipManyTill anyChar (char '>')

pNonTag :: Parsec TL.Text () Builder
pNonTag = fromString <$> many1 (satisfy (/='<'))

getInnerText :: TL.Text -> Builder
getInnerText t =
  case parse pInnerText "" t of
    Left _  -> fromLazyText t
    Right b -> b
