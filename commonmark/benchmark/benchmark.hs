{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
import Test.Tasty.Bench
import Data.Text (Text)
import Data.Functor.Identity  -- base >= 4.8
import Commonmark
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  sample <- T.replicate 10 <$> TIO.readFile "benchmark/sample.md"
  defaultMain
    [ bgroup "tokenize"
      [ benchTokenize ("sample.md", sample) ]
    , bgroup "parse sample.md"
      [ benchCommonmark defaultSyntaxSpec ("commonmark default", sample)
      ]
    , bgroup "pathological"
      (map toPathBench pathtests)
    , bgroup "name impact"
      (map (toNameImpactBench sample) nameImpactTests)
    ]

toPathBench :: (String, Int -> T.Text) -> Benchmark
toPathBench (name, ptest) =
  bgroup name
  [ bgroup "commonmark"
    (map (\n -> benchCommonmark defaultSyntaxSpec (show n, ptest n))
      [1000, 2000, 3000, 4000])
  ]

pathtests :: [(String, Int -> T.Text)]
pathtests =
  [ ("nested strong emph", \n ->
     let num = n `div` 14 in
     T.replicate num "*a **a " <> "b" <>
     T.replicate num " a** a*")
  , ("many emph closers with no openers", \n ->
     let num = n `div` 3 in
     T.replicate num "a_ ")
  , ("many emph openers with no closers", \n ->
     let num = n `div` 3 in
     T.replicate num "_a ")
  , ("many link closers with no openers", \n ->
    let num = n `div` 2 in
     T.replicate num "a]")
  , ("many link openers with no closers", \n ->
    let num = n `div` 2 in
     T.replicate num "[a")
  , ("mismatched openers and closers", \n ->
    let num = n `div` 3 in
     T.replicate num "*a_ ")
  , ("openers and closers multiple of 3", \n ->
    let num = n `div` 7 in
     T.replicate num "a**b" <> T.replicate num "c* ")
  , ("link openers and emph closers", \n ->
    let num = n `div` 4 in
     mconcat (replicate num "[ a_"))
  , ("nested brackets", \n ->
    let num = n `div` 2 in
     T.replicate num "[" <> "a" <> T.replicate num "]")
  , ("inline link openers without closers", \n ->
    let num = n `div` 3 in
    T.replicate num "[](")
  , ("repeated pattern '[ (]('" , \n ->
    let num = n `div` 5 in
    T.replicate num "[ (](")
  , ("nested block quotes", \n ->
    let num = n `div` 2 in
     T.replicate num "> " <> "a")
  , ("nested list", \n ->
    let num = floor (sqrt (fromIntegral n :: Double)) in
    mconcat (map (\ind -> T.replicate ind " " <> "- a\n") [0..(num - 1)]))
  , ("nested list 2", \n ->
    let num = n `div` 2 in
    T.replicate num "* " <> "a\n")
  , ("backticks", \n ->
    let num = floor (sqrt (9 + (8 * (fromIntegral n :: Double))) / 2) in
    mconcat $ map (\x -> "e" <> T.replicate x "`") [1..num])
  , ("CDATA", \n ->
    let num = n `div` 11 in
    T.replicate num "a <![CDATA[")
  , ("<?", \n ->
    let num = n `div` 2 in
    ("a" <> T.replicate num "<?"))
  , ("<!A ", \n ->
    let num = n `div` 4 in
    ("a" <> T.replicate num "<!A "))
  ]

toNameImpactBench :: Text -> (String, String) -> Benchmark
toNameImpactBench sample (testName, name) =
  let benchArgs n = (show n, take (50 * n) (cycle name), sample)
   in bgroup testName
        (map (benchCommonmark' @SourceRange defaultSyntaxSpec . benchArgs)
          [1, 5, 10, 20])

nameImpactTests :: [(String, String)]
nameImpactTests =
  [ ("no special characters", "the quick brown fox jumps over the lazy dog")
  , ("special characters", "\\-:-as;df-:d:%%-:\\;;;\\-:%%-:---:-sdf-:sa-\\;")
  ]

benchCommonmark :: SyntaxSpec Identity (Html ()) (Html ())
                -> (String, Text)
                -> Benchmark
benchCommonmark spec (name, contents) =
  benchCommonmark' spec (name, name, contents)

benchCommonmark' :: Rangeable (Html a)
                => SyntaxSpec Identity (Html a) (Html a)
                -> (String, String, Text)
                -> Benchmark
benchCommonmark' spec (testName, name, contents) =
  bench testName $
    nf (either (error . show) renderHtml
        . runIdentity . parseCommonmarkWith spec . tokenize name)
    contents

benchTokenize :: (String, Text) -> Benchmark
benchTokenize (name, contents) =
  bench ("tokenize " ++ name) $ nf (length . tokenize name) contents
