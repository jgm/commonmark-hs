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
  , SourcePos
  , Rangeable(..)
  )
where
import           Data.Data            (Data)
import           Data.Foldable        (foldMap)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Read       as TR
import           Data.Text.Encoding   (encodeUtf8)
import qualified Data.ByteString.Char8 as B
import           Data.Semigroup       (Semigroup, (<>))
import           Data.Typeable        (Typeable)
import           Text.Parsec.Pos      (SourcePos, sourceColumn, sourceLine,
                                       sourceName)
import           Data.Text.Lazy.Builder (Builder, singleton, fromText,
                                         toLazyText, fromString)
import           Text.Printf          (printf)
import           Data.Char            (ord, isAlphaNum, isAscii, isSpace)

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
  lineBreak = fromText "<br />\n"
  softBreak = singleton '\n'
  str t = escapeHtml t
  entity t
    | illegalCodePoint t = fromText "\xFFFD"
    | otherwise = fromText t
  escapedChar c = escapeHtmlChar c
  emph ils = fromText "<em>" <> ils <> fromText "</em>"
  strong ils = fromText "<strong>" <> ils <> fromText "</strong>"
  link target title ils = fromText "<a href=\"" <>
    escapeURI target <> fromText "\"" <>
    (if T.null title
        then mempty
        else fromText " title=\"" <> escapeHtml title <> fromText "\"") <>
    fromText ">" <> ils <> fromText "</a>"
  image target title ils = fromText "<img src=\"" <>
    escapeURI target <> fromText "\"" <>
    fromText " alt=\"" <> innerText ils <> fromText "\"" <>
    (if T.null title
        then mempty
        else fromText " title=\"" <> escapeHtml title <> fromText "\"") <>
    fromText " />"
  code t = fromText "<code>" <> escapeHtml t <> fromText "</code>"
  rawInline f t
    | f == Format "html" = fromText t
    | otherwise          = mempty

escapeHtml :: Text -> Builder
escapeHtml = foldMap escapeHtmlChar . T.unpack

escapeHtmlChar :: Char -> Builder
escapeHtmlChar '<' = fromText "&lt;"
escapeHtmlChar '>' = fromText "&gt;"
escapeHtmlChar '&' = fromText "&amp;"
escapeHtmlChar '"' = fromText "&quot;"
escapeHtmlChar c   = singleton c

escapeURI :: Text -> Builder
escapeURI = foldMap escapeURIChar . B.unpack . encodeUtf8

escapeURIChar :: Char -> Builder
escapeURIChar c
  | isEscapable c = singleton '%' <> fromString (printf "%02X" (ord c))
  | otherwise     = singleton c
  where isEscapable d = not (isAscii d && isAlphaNum d)
                     && d `notElem` ['%','/','?',':','@','-','.','_','~','&',
                                     '!','$','\'','(',')','*','+',',',';','=']

innerText :: Builder -> Builder
innerText = getInnerText . toLazyText

getInnerText :: TL.Text -> Builder
getInnerText = snd . TL.foldl' f (False, mempty)
  where f :: (Bool, Builder) -> Char -> (Bool, Builder)
        f (False, b) '<' = (True, b)
        f (True, b) '>'  = (False, b)
        f (True, b) _    = (True, b)
        f (False, b) c   = (False, b <> singleton c)

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
  paragraph ils = fromText "<p>" <> ils <> fromText "</p>" <> nl
  plain ils = ils <> nl
  thematicBreak = fromText "<hr />" <> nl
  blockQuote bs = fromText "<blockquote>" <> nl <> bs <>
    fromText "</blockquote>" <> nl
  codeBlock info t = fromText "<pre><code" <>
    (if T.null lang
        then mempty
        else fromText (" class=\"language-" <> lang <> "\"")) <>
    escapeHtml t <> fromText "</code></pre>" <> nl
    where lang = T.takeWhile (not . isSpace) info
  header level ils = singleton '<' <> h <> singleton '>' <>
    ils <> fromText "</" <> h <> singleton '>' <> nl
    where h = fromText $
               case level of
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
  list (BulletList _) lSpacing items = fromText "<ul>" <> nl <>
    mconcat (map li items) <> fromText "</ul>" <> nl
   where li x = fromText "<li>" <> x <>
                (if lSpacing == TightList then mempty else nl) <>
                fromText "</li>" <> nl
  list (OrderedList startnum _) lSpacing items = fromText "<ol" <>
    (if startnum /= 1
        then fromText " start=\"" <> fromString (show startnum) <> fromText "\""
        else mempty) <> fromText ">" <> nl <>
    mconcat (map li items) <> fromText "</ol>" <> nl
   where li x = fromText "<li>" <> x <>
                (if lSpacing == TightList then mempty else nl) <>
                fromText "</li>" <> nl

nl :: Builder
nl = fromText "\n"

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
