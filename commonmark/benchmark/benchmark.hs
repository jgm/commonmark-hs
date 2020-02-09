{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Criterion.Main
import Data.Functor.Identity  -- base >= 4.8
import Commonmark
import Commonmark.Html
import qualified Data.ByteString as B
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

main :: IO ()
main = do
  sample <- mconcat . replicate 10 <$> B.readFile "benchmark/sample.md"
  defaultMainWith defaultConfig
    [ bgroup "tokenize"
      [ benchTokenize ("sample.md", sample) ]
    , bgroup "parse sample.md"
      [ benchCommonmark defaultSyntaxSpec ("commonmark default", sample)
      -- , benchCommonmark (smartPunctuationSpec <> defaultSyntaxSpec)
      --     ("commonmark +smart", sample)
      -- , benchCommonmark (autolinkSpec <> defaultSyntaxSpec)
      --     ("commonmark +autolink", sample)
      -- , benchCommonmark (attributesSpec <> defaultSyntaxSpec)
      --     ("commonmark +attributes", sample)
      -- , benchCommonmark (defaultSyntaxSpec <> pipeTableSpec)
      --     ("commonmark +pipe_table", sample)
      ]
    , bgroup "pathological"
      (map toPathBench pathtests)
    ]

toPathBench :: (String, Int -> B.ByteString) -> Benchmark
toPathBench (name, ptest) =
  bgroup name
  [ bgroup "commonmark"
    (map (\n -> benchCommonmark defaultSyntaxSpec (show n, ptest n))
      [1000, 2000, 3000, 4000])
  ]

pathtests :: [(String, Int -> B.ByteString)]
pathtests =
  [ ("nested strong emph", \n ->
     let num = n `div` 14 in
     mconcat (replicate num "*a **a ") <> "b" <>
     mconcat (replicate num " a** a*"))
  , ("many emph closers with no openers", \n ->
     let num = n `div` 3 in
     mconcat (replicate num "a_ "))
  , ("many emph openers with no closers", \n ->
     let num = n `div` 3 in
     mconcat (replicate num "_a "))
  , ("many link closers with no openers", \n ->
    let num = n `div` 2 in
     mconcat (replicate num "a]"))
  , ("many link openers with no closers", \n ->
    let num = n `div` 2 in
     mconcat (replicate num "[a"))
  , ("mismatched openers and closers", \n ->
    let num = n `div` 3 in
     mconcat (replicate num "*a_ "))
  , ("openers and closers multiple of 3", \n ->
    let num = n `div` 7 in
     mconcat (replicate num "a**b") <> mconcat (replicate num "c* "))
  , ("link openers and emph closers", \n ->
    let num = n `div` 4 in
     mconcat (replicate num "[ a_"))
  , ("nested brackets", \n ->
    let num = n `div` 2 in
     mconcat (replicate num "[") <> "a" <> mconcat (replicate num "]"))
  , ("inline link openers without closers", \n ->
    let num = n `div` 3 in
    mconcat (replicate num "[]("))
  , ("repeated pattern '[ (]('" , \n ->
    let num = n `div` 5 in
    mconcat (replicate num "[ (]("))
  , ("nested block quotes", \n ->
    let num = n `div` 2 in
     mconcat (replicate num "> ") <> "a")
  , ("nested list", \n ->
    let num = floor (sqrt (fromIntegral n :: Double)) in
    mconcat (map (\ind -> mconcat (replicate ind " ") <> "- a\n") [0..(num - 1)]))
  , ("nested list 2", \n ->
    let num = n `div` 2 in
    mconcat (replicate num "* ") <> "a\n")
  , ("backticks", \n ->
    let num = floor (sqrt (9 + (8 * (fromIntegral n :: Double))) / 2) in
    mconcat $ map (\x -> "e" <> mconcat (replicate x "`")) [1..num])
  ]

benchCommonmark :: SyntaxSpec Identity (Html ()) (Html ())
                -> (String, B.ByteString)
                -> Benchmark
benchCommonmark spec (name, contents) =
  bench name $
    nf (either (error . show) renderHtml
        . runIdentity . parseCommonmarkWith spec . tokenize name)
    contents

benchTokenize :: (String, B.ByteString) -> Benchmark
benchTokenize (name, contents) =
  bench ("tokenize " ++ name) $ nf (length . tokenize name) contents
