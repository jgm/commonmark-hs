{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Html
  ( escapeURI
  , escapeHtml
  , escapeHtmlChar
  , innerText
  )
where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Lazy.Builder (Builder, singleton,
                                         toLazyText, fromString,
                                         fromLazyText)
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup       ((<>))
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii)
import           Commonmark.Entity    (unEntity)
import           Commonmark.Tokens    (tokenize)
import           Text.Parsec
import           Commonmark.Util      (skipManyTill)

escapeHtml :: Text -> Builder
escapeHtml = foldMap escapeHtmlChar . T.unpack

escapeHtmlChar :: Char -> Builder
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = singleton c

escapeURI :: Text -> Builder
escapeURI = foldMap escapeURIChar . B.unpack .
  encodeUtf8 . unEntity . tokenize "URI"

escapeURIChar :: Char -> Builder
escapeURIChar c
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
