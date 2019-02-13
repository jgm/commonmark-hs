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
                                         toLazyText, fromString)
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup       ((<>))
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii)
import           Commonmark.Entity    (unEntity)
import           Commonmark.Tokens    (tokenize)
import           Commonmark.Util      (skipManyTill)
import           Commonmark.ParserCombinators

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
innerText = getInnerText . TL.unpack . toLazyText

pInnerText :: Parser Char () Builder
pInnerText = (mconcat <$> many (pTag <|> pNonTag)) <* eof

char :: Char -> Parser Char () Char
char c = satisfy (== c)

anyChar :: Parser Char () Char
anyChar = satisfy (const True)

string :: String -> Parser Char () String
string s = sequence $ map char s

pTag :: Parser Char () Builder
pTag = do
  char '<'
  cs <- many (satisfy isAlphaNum)
  if cs == "img" || cs == "IMG"
     then do
       alt <- option "" $ do
                skipManyTill anyChar (string "alt=\"")
                manyTill anyChar (char '"')
       skipManyTill anyChar (char '>')
       return $ fromString alt
     else mempty <$ skipManyTill anyChar (char '>')

pNonTag :: Parser Char () Builder
pNonTag = fromString <$> some (satisfy (/='<'))

getInnerText :: String -> Builder
getInnerText t =
  case runParser pInnerText () t of
    Left _  -> fromString t
    Right b -> b
