{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Commonmark
import           Control.Monad         (when)
import           Data.Functor.Identity
import           Data.List             (groupBy)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.IO             (hSetEncoding, utf8, openFile,
                                        IOMode(..))
import qualified Data.Text.Lazy        as TL
import           Data.Text.Normalize   (normalize, NormalizationMode(NFC))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.Parsec
import           Text.Parsec.Pos

readTextFile :: FilePath -> IO Text
readTextFile fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  T.hGetContents h

main :: IO ()
main = do
  let defaultParser = runIdentity . parseCommonmarkWith defaultSyntaxSpec
  tests <- mapM (uncurry getSpecTestTree)
             [ ("test/spec.txt", mempty)
             , ("test/regression.md", mempty)
             ]
  defaultMain $ testGroup "Tests"
     (testProperty "tokenize/untokenize roundtrip" tokenize_roundtrip
      : pathologicalTests defaultParser
      : toSpecTest defaultParser
        SpecTest
          { section    = "Issue #24 (eof after HTML block)"
          , example    = 1
          , markdown   = "<? a ?>"
          , end_line   = 1
          , start_line = 1
          , html       = "<? a ?>" }
      : toSpecTest defaultParser
        SpecTest
          { section    = "Issue #24 (eof after HTML block)"
          , example    = 2
          , markdown   = "<!-- a -->"
          , end_line   = 2
          , start_line = 2
          , html       = "<!-- a -->" }
      : tests)

getSpecTestTree :: FilePath
                -> SyntaxSpec Identity (Html ()) (Html ())
                -> IO TestTree
getSpecTestTree fp syntaxspec = do
  spectests <- getSpecTests fp
  let spectestgroups = groupBy (\t1 t2 -> section t1 == section t2)
                          spectests
  let spectestsecs = [(section x, xs) | xs@(x:_) <- spectestgroups]
  let parser = runIdentity . parseCommonmarkWith
                   (syntaxspec <> defaultSyntaxSpec)
  return $ testGroup fp $
    map (\(secname, tests) ->
           testGroup (T.unpack secname) $
             map (toSpecTest parser) tests)
        spectestsecs

getSpecTests :: FilePath -> IO [SpecTest]
getSpecTests fp = do
  speclines <- zip [1..] . T.lines . T.replace "→" "\t"
                <$> readTextFile fp
  return $ either (error . show) id $ runParser
             (many (try (skipMany normalLine *> parseSpecTest))
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
          expected = normalizeHtml $ html st
          actual = normalizeHtml .  TL.toStrict . renderHtml .
                   fromRight mempty $
                     (parser (tokenize "" (markdown st))
                      :: Either ParseError (Html ()))

-- Pathological tests, ported from commonmark.js's test/test.js.
-- Each case must produce the expected output within the timeout;
-- a timeout indicates nonlinear (typically quadratic) behavior.
pathologicalTests :: ([Tok] -> Either ParseError (Html ()))
                  -> TestTree
pathologicalTests parser =
  localOption (mkTimeout (5 * 1000000)) $  -- 5 seconds per case
  testGroup "Pathological cases" $
    map toPathTest pathologicalCases
 where
  toPathTest (name, inp, expected) =
    testCase name $
      (normalizeHtml . TL.toStrict . renderHtml . fromRight mempty)
        (parser (tokenize "" inp))
      @?= normalizeHtml expected

pathologicalCases :: [(String, Text, Text)]
pathologicalCases =
    [ ("U+0000 in input",
       "abc\0xyz\0\n",
       "<p>abc\65533\&xyz\65533</p>\n")
    , ("alternate line endings",
       "- a\n- b\r- c\r\n- d",
       "<ul>\n<li>a</li>\n<li>b</li>\n<li>c</li>\n<li>d</li>\n</ul>\n")
    , ("paragraph of 200000 words",
       rep 200000 "lorem ",
       "<p>" <> rep 199999 "lorem " <> "lorem</p>\n")
    ] ++
    concatMap forSize [1000, 10000] ++
    map backslashTitle [10, 100, 1000]
 where
  rep = T.replicate
  forSize :: Int -> [(String, Text, Text)]
  forSize x =
    let sx = show x
        n = rep x
    in
    [ ("nested strong emph " <> sx <> " deep",
       n "*a **a " <> "b" <> n " a** a*",
       "<p>" <> n "<em>a <strong>a " <> "b" <>
         n " a</strong> a</em>" <> "</p>\n")
    , (sx <> " emph closers with no openers",
       n "a_ ",
       "<p>" <> rep (x - 1) "a_ " <> "a_</p>\n")
    , (sx <> " emph openers with no closers",
       n "_a ",
       "<p>" <> rep (x - 1) "_a " <> "_a</p>\n")
    , (sx <> " openers and closers multiple of 3",
       "a**b" <> n "c* ",
       "<p>a**b" <> rep (x - 1) "c* " <> "c*</p>\n")
    , (sx <> " #172",
       n "*_* _ ",
       "<p>" <> rep (x - 1) "<em>_</em> _ " <> "<em>_</em> _</p>\n")
    , (sx <> " link closers with no openers",
       n "a] ",
       "<p>" <> rep (x - 1) "a] " <> "a]</p>\n")
    , (sx <> " link openers with no closers",
       n "[a ",
       "<p>" <> rep (x - 1) "[a " <> "[a</p>\n")
    , (sx <> " link openers and emph closers",
       n "[ a_ ",
       "<p>" <> rep (x - 1) "[ a_ " <> "[ a_</p>\n")
    , (sx <> " mismatched openers and closers",
       n "*a_ ",
       "<p>" <> rep (x - 1) "*a_ " <> "*a_</p>\n")
    , (sx <> " pattern [ (](",
       n "[ (](",
       "<p>" <> n "[ (](" <> "</p>\n")
    , ("nested brackets " <> sx <> " deep",
       n "[" <> "a" <> n "]",
       "<p>" <> n "[" <> "a" <> n "]" <> "</p>\n")
    , ("nested block quote " <> sx <> " deep",
       n "> " <> "a\n",
       n "<blockquote>\n" <> "<p>a</p>\n" <> n "</blockquote>\n")
    , ("[\\\\... " <> sx <> " deep",
       "[" <> n "\\" <> "\n",
       "<p>[" <> rep (x `div` 2) "\\" <> "</p>\n")
    ]
  backslashTitle x =
    (show x <> " backslashes in unclosed link title",
     "[test](\\url \"" <> rep x "\\" <> "\n",
     "<p>[test](\\url &quot;" <> rep (x `div` 2) "\\" <> "</p>\n")

normalizeHtml :: Text -> Text
normalizeHtml = T.replace "\n</li>" "</li>" .
                T.replace "<li>\n" "<li>"

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x

tokenize_roundtrip :: String -> Bool
tokenize_roundtrip s = untokenize (tokenize "source" t) == t
  where t = T.replace "\0" "\xFFFD" . normalize NFC $ T.pack s

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
  markdownTxt <- T.unlines <$> manyTill (satisfyLine (const True))
                                 (satisfyLine (=="."))
  htmlTxt <- T.unlines <$> manyTill (satisfyLine (const True))
              (satisfyLine (== "````````````````````````````````"))
  endline <- (\x -> x - 1) . sourceLine <$> getPosition
  (sectionName, exampleNumber) <- getState
  putState (sectionName, exampleNumber + 1)
  return SpecTest{
       section = sectionName
     , example = exampleNumber
     , markdown = markdownTxt
     , end_line = endline
     , start_line = sourceLine startpos
     , html = htmlTxt
   }

normalLine :: Parsec [(Int, Text)] (Text, Int) ()
normalLine = do
  t <- satisfyLine (/= "```````````````````````````````` example")
  when ("#" `T.isPrefixOf` t) $ updateState $ \(_secname, exampnum) ->
           (T.strip $ T.dropWhile (=='#') t, exampnum)

---
