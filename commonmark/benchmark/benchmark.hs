{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Criterion.Main
import Data.Text (Text)
import Commonmark
import Commonmark.Inlines
import Lucid (renderText)
import qualified Data.Text as T
import qualified Data.Text.IO as T
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import qualified CMark as CMark
import Text.Blaze.Html.Renderer.Text as Blaze
import qualified Cheapskate as Cheapskate
import qualified Cheapskate.Html as CheapskateHtml
import Lucid

main :: IO ()
main = do
  sample <- T.readFile "benchmark/sample.md"
  defaultMainWith defaultConfig
    [ bgroup "tokenize"
      [ benchTokenize ("sample.md", sample) ]
    , bgroup "parseChunks"
      [ benchChunks ("sample.md", sample) ]
    , bgroup "parse sample.md"
      [ benchCommonmark ("commonmark", sample)
      , benchCMark ("cmark", sample)
      , benchCheapskate ("cheapskate", sample) ]
    , bgroup "pathological"
      (map toPathBench pathtests)
    ]

toPathBench :: (String, Int -> T.Text) -> Benchmark
toPathBench (name, ptest) =
  bgroup name
  [ bgroup "commonmark"
    (map (\n -> benchCommonmark (show n, ptest n)) [800, 1200, 1600, 2000])
  -- , bgroup "cmark"
  --  (map (\n -> benchCMark (show n, ptest n)) [400, 800, 1200, 1600, 2000])
  -- , bgroup "cheapskate"
  --  (map (\n -> benchCheapskate (show n, ptest n)) [300, 600, 900])
  -- , bgroup "comark"
  -- (map (\n -> benchComark (show n, ptest n)) [800, 1200, 1600, 2000])
  ]

pathtests :: [(String, Int -> T.Text)]
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
  , ("nested block quotes", \n ->
    let num = n `div` 2 in
     mconcat (replicate num "> ") <> "a")
  , ("backticks", \n ->
    let num = floor (sqrt (9 + (8 * (fromIntegral n :: Double))) / 2) in
     mconcat $ map (\x -> "e" <> mconcat (replicate x "`")) [1..num])
  ]

benchCMark :: (String, Text) -> Benchmark
benchCMark (name, contents) =
  bench name $
    nf (T.length . CMark.commonmarkToHtml []) contents

-- benchComark :: (String, Text) -> Benchmark
-- benchComark (name, contents) =
--   bench name $
--     nf (T.length . Comark.render . Comark.parse [])
--     contents

benchCheapskate :: (String, Text) -> Benchmark
benchCheapskate (name, contents) =
  bench name $
    nf (Blaze.renderHtml . CheapskateHtml.renderDoc
                   . Cheapskate.markdown Cheapskate.def) contents

benchCommonmark :: (String, Text) -> Benchmark
benchCommonmark (name, contents) =
  bench name $
    nf (either (error . show) renderText
        . parseCommonmark . tokenize name)
    contents

benchTokenize :: (String, Text) -> Benchmark
benchTokenize (name, contents) =
  bench ("tokenize " ++ name) $ nf (length . tokenize name) contents

benchChunks :: (String, Text) -> Benchmark
benchChunks (name, contents) =
  bench name $ nfIO $ do
    res <- parseChunks defaultBracketedSpecs defaultFormattingSpecs
             defaultInlineParsers (tokenize name contents)
    case res of
         Left e -> error (show e)
         Right (cs :: [Chunk (Html ())]) -> return $ length cs
