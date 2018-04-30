{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Commonmark.Parser
import           Commonmark.Extensions.Autolink
import           Commonmark.Extensions.PipeTable
import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.Strikethrough
import           Commonmark.Extensions.Math
import           Control.Monad         (when)
import           Data.Functor.Identity
import           Data.List             (groupBy)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy        as TL
import           Data.Text.Lazy.Builder (toLazyText, Builder)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Parsec
import           Text.Parsec.Pos

main :: IO ()
main = do
  spectests <- getSpecTestTree "test/spec.txt" defaultSyntaxSpec
  smarttests <- getSpecTestTree "test/smart_punct.txt"
                   (smartPunctuationSpec <> defaultSyntaxSpec)
  strikethroughtests <- getSpecTestTree "test/strikethrough.txt"
                         (strikethroughSpec <> defaultSyntaxSpec)
  pipetabletests <- getSpecTestTree "test/pipe-tables.txt"
                         (pipeTableSpec <> defaultSyntaxSpec)
  mathtests <- getSpecTestTree "test/math.txt"
                         (mathSpec <> defaultSyntaxSpec)
  autolinktests <- getSpecTestTree "test/autolinks.txt"
                         (autolinkSpec <> defaultSyntaxSpec)
  defaultMain $ testGroup "Tests"
    [ testProperty "tokenize/untokenize roundtrip" tokenize_roundtrip
    , spectests
    , smarttests
    , strikethroughtests
    , pipetabletests
    , mathtests
    , autolinktests
    -- we handle these in the benchmarks now
    -- , testGroup "Pathological tests" $
    --    map pathologicalTest pathtests
    ]

getSpecTestTree :: FilePath
                -> SyntaxSpec Identity Builder Builder
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

toSpecTest :: ([Tok] -> Either ParseError Builder)
           -> SpecTest -> TestTree
toSpecTest parser st =
  testCase name (actual @?= expected)
    where name = T.unpack (section st) ++ " example " ++ show (example st) ++
                 " (" ++ show (start_line st) ++ "-" ++
                 show (end_line st) ++ ")"
          expected = normalizeHtml $ html st
          actual = normalizeHtml .  TL.toStrict . toLazyText .
                   fromRight mempty $
                     (parser (tokenize "" (markdown st))
                      :: Either ParseError Builder)

normalizeHtml :: Text -> Text
normalizeHtml = T.replace "\n</li>" "</li>" .
                T.replace "<li>\n" "<li>"

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x

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
