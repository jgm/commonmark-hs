{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Commonmark
import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.Strikethrough
import           Commonmark.Extensions.PipeTable
import           Control.Monad         (when)
import           Data.Functor.Identity
import           Data.List             (sort, groupBy)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy        as TL
import           Lucid
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.HTML.TagSoup
import           Text.Parsec
import           Text.Parsec.Pos

main :: IO ()
main = do
  spectests <- getSpecTestTree "test/spec.txt" defaultSyntaxSpec
  smarttests <- getSpecTestTree "test/smart_punct.txt"
                   (defaultSyntaxSpec <> smartPunctuationSpec)
  strikethroughtests <- getSpecTestTree "test/strikethrough.txt"
                         (defaultSyntaxSpec <> strikethroughSpec)
  pipetabletests <- getSpecTestTree "test/pipe-tables.txt"
                         (defaultSyntaxSpec <> pipeTableSpec)
  defaultMain $ testGroup "Tests"
    [ testProperty "tokenize/untokenize roundtrip" tokenize_roundtrip
    , spectests
    , smarttests
    , strikethroughtests
    , pipetabletests
    -- we handle these in the benchmarks now
    -- , testGroup "Pathological tests" $
    --    map pathologicalTest pathtests
    ]

getSpecTestTree :: FilePath
                -> SyntaxSpec Identity (Html ()) (Html ())
                -> IO TestTree
getSpecTestTree fp syntaxspec = do
  spectests <- getSpecTests fp
  let spectestgroups = groupBy (\t1 t2 -> section t1 == section t2)
                          spectests
  let spectestsecs = [(section (head xs), xs) | xs <- spectestgroups]
  let parser = runIdentity . parseCommonmarkWith syntaxspec
  return $ testGroup fp $
    map (\(secname, tests) ->
           testGroup (T.unpack secname) $
             map (toSpecTest parser) tests)
        spectestsecs

getSpecTests :: FilePath -> IO [SpecTest]
getSpecTests fp = do
  speclines <- zip [1..] . T.lines . T.replace "→" "\t"
                <$> T.readFile fp
  return $ either (error . show) id $ runParser
             (many1 (try (skipMany normalLine *> parseSpecTest))
                <* skipMany normalLine <* eof) ("",1) fp
                speclines

data SpecTest = SpecTest
     { section    :: Text
     , example    :: Int
     , markdown   :: Text
     , end_line   :: Int
     , start_line :: Int
     , html       :: Text }
  deriving (Show)

toSpecTest :: ([Tok] -> Either ParseError (Html ()))
           -> SpecTest -> TestTree
toSpecTest parser st =
  testCase name (actual @?= expected)
    where name = T.unpack (section st) ++ " example " ++ show (example st) ++
                 " (" ++ show (start_line st) ++ "-" ++
                 show (end_line st) ++ ")"
          expected = normalizeHTML $ html st
          actual = normalizeHTML $
                   TL.toStrict . renderText .
                   fromRight mempty $
                     (parser (tokenize "" (markdown st))
                      :: Either ParseError (Html ()))

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x

normalizeHTML :: Text -> Text
normalizeHTML = renderTagsOptions renderOptions{
                    optEscape = escapeHTML' } .
                 map normalizeTag .
                 removeNewlineWithLi .
                 filter (\t -> not $
                           isTagCloseName "img" t ||
                           isTagCloseName "hr" t ||
                           isTagCloseName "br" t) .
                 parseTags
  where escapeHTML' = T.replace "'" "&#39;" . escapeHTML

removeNewlineWithLi :: [Tag Text] -> [Tag Text]
removeNewlineWithLi [] = []
removeNewlineWithLi (t@(TagOpen "li" _) : TagText "\n" : ts) =
  t : removeNewlineWithLi ts
removeNewlineWithLi (TagText txt : t@(TagClose "li") : ts)
  | txt == "\n" = t : removeNewlineWithLi ts
  | T.last txt == '\n' = TagText (T.init txt) : t : removeNewlineWithLi ts
removeNewlineWithLi (t:ts) = t : removeNewlineWithLi ts

normalizeTag :: Tag Text -> Tag Text
normalizeTag (TagOpen name attrs) = TagOpen name (sort attrs)
normalizeTag x                    = x

tokenize_roundtrip :: String -> Bool
tokenize_roundtrip s = untokenize (tokenize "source" t) == t
  where t = T.pack s

--- parser for spec test cases

satisfyLine :: (Text -> Bool)
            -> Parsec [(Int, Text)] (Text, Int) Text
satisfyLine f = token showTok posFromTok testTok
  where
     showTok (_,t)       = T.unpack t
     posFromTok (pos,_)  = newPos "" pos 1
     testTok (_,t)       = if f t then Just t else Nothing

parseSpecTest :: Parsec [(Int, Text)] (Text, Int) SpecTest
parseSpecTest = do
  startpos <- getPosition
  () <$ satisfyLine (== "```````````````````````````````` example")
  markdownText <- T.unlines <$> manyTill (satisfyLine (const True))
                                 (satisfyLine (=="."))
  htmlText <- T.unlines <$> manyTill (satisfyLine (const True))
              (satisfyLine (== "````````````````````````````````"))
  endline <- (\x -> x - 1) . sourceLine <$> getPosition
  (sectionName, exampleNumber) <- getState
  putState (sectionName, exampleNumber + 1)
  return SpecTest{
       section = sectionName
     , example = exampleNumber
     , markdown = markdownText
     , end_line = endline
     , start_line = sourceLine startpos
     , html = htmlText
   }

normalLine :: Parsec [(Int, Text)] (Text, Int) ()
normalLine = do
  t <- satisfyLine (/= "```````````````````````````````` example")
  when ("#" `T.isPrefixOf` t) $ updateState $ \(_secname, exampnum) ->
           (T.strip $ T.dropWhile (=='#') t, exampnum)

---
