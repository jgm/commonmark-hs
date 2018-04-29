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

escapeHtml :: Text -> Builder
escapeHtml = foldMap escapeHtmlChar . T.unpack

escapeHtmlChar :: Char -> Builder
escapeHtmlChar '<' = "&lt;"
escapeHtmlChar '>' = "&gt;"
escapeHtmlChar '&' = "&amp;"
escapeHtmlChar '"' = "&quot;"
escapeHtmlChar c   = singleton c

escapeURI :: Text -> Builder
escapeURI = foldMap escapeURIChar . B.unpack . encodeUtf8

escapeURIChar :: Char -> Builder
escapeURIChar c
  | isEscapable c = singleton '%' <> fromString (printf "%02X" (ord c))
  | otherwise     = singleton c
  where isEscapable d = not (isAscii d && isAlphaNum d)
                     && d `notElem` ['%','/','?',':','@','-','.','_','~','&',
                                     '#','!','$','\'','(',')','*','+',',',
                                     ';','=']

innerText :: Builder -> Builder
innerText = getInnerText . toLazyText

getInnerText :: TL.Text -> Builder
getInnerText = snd . TL.foldl' f (False, mempty)
  where f :: (Bool, Builder) -> Char -> (Bool, Builder)
        f (False, b) '<' = (True, b)
        f (True, b) '>'  = (False, b)
        f (True, b) _    = (True, b)
        f (False, b) c   = (False, b <> singleton c)

