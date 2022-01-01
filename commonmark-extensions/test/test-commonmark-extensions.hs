{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Commonmark
import           Commonmark.Extensions
import           Control.Monad         (when)
import           Data.Functor.Identity
import           Data.List             (groupBy)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           System.IO             (hSetEncoding, utf8, openFile,
                                        IOMode(..))
import qualified Data.Text.Lazy        as TL
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.Parsec
import           Text.Parsec.Pos

readTextFile :: FilePath -> IO Text
readTextFile fp = do
  h <- openFile fp ReadMode
  hSetEncoding h utf8
  T.hGetContents h

main :: IO ()
main = do
  tests <- mapM (uncurry getSpecTestTree)
             [ ("test/smart.md", smartPunctuationSpec)
             , ("test/hard_line_breaks.md", hardLineBreaksSpec)
             , ("test/strikethrough.md", strikethroughSpec)
             , ("test/superscript.md", superscriptSpec)
             , ("test/subscript.md", subscriptSpec)
             , ("test/pipe_tables.md", pipeTableSpec)
             , ("test/footnotes.md", footnoteSpec)
             , ("test/math.md", mathSpec)
             , ("test/emoji.md", emojiSpec)
             , ("test/autolinks.md", autolinkSpec)
             , ("test/definition_lists.md", definitionListSpec)
             , ("test/fancy_lists.md", fancyListSpec)
             , ("test/task_lists.md", taskListSpec)
             , ("test/attributes.md", attributesSpec)
             , ("test/raw_attribute.md", rawAttributeSpec)
             , ("test/bracketed_spans.md", bracketedSpanSpec)
             , ("test/fenced_divs.md", fencedDivSpec)
             , ("test/auto_identifiers.md", autoIdentifiersSpec <> attributesSpec)
             , ("test/implicit_heading_references.md",
                 autoIdentifiersSpec <> attributesSpec <> implicitHeadingReferencesSpec)
             , ("test/wikilinks_title_before_pipe.md", wikilinksSpec TitleBeforePipe)
             , ("test/wikilinks_title_after_pipe.md", wikilinksSpec TitleAfterPipe)
             ]
  defaultMain $ testGroup "Tests" (tests ++ [rebaseRelativePathTests])

getSpecTestTree :: FilePath
                -> SyntaxSpec Identity (Html ()) (Html ())
                -> IO TestTree
getSpecTestTree fp syntaxspec = do
  spectests <- getSpecTests fp
  let spectestgroups = groupBy (\t1 t2 -> section t1 == section t2)
                          spectests
  let spectestsecs = [(section (head xs), xs) | xs <- spectestgroups]
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

normalizeHtml :: Text -> Text
normalizeHtml = T.replace "\n</li>" "</li>" .
                T.replace "<li>\n" "<li>"

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x

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

rebaseRelativePathTests :: TestTree
rebaseRelativePathTests = do
  let parser = runIdentity . parseCommonmarkWith
                   (rebaseRelativePathsSpec <> defaultSyntaxSpec)
  let md = T.unlines
            [ "![image](foo.jpg)"
            , "[link](http://example.com/foo.jpg)"
            , "![image]()"
            , "[link](#foobar)"
            , "![image][ref]"
            , "![image](/absolute/path.jpg)"
            , ""
            ]
  let mdref = "[ref]: baz.png"
  let toks = tokenize "chap1/text.md" md ++ tokenize "extra/refs.md" mdref
  let actual = normalizeHtml .  TL.toStrict . renderHtml .
                  fromRight mempty $ (parser toks
                       :: Either ParseError (Html ()))
  let expected = T.unlines
                 [ "<p><img src=\"chap1/foo.jpg\" alt=\"image\" />"
                 , "<a href=\"http://example.com/foo.jpg\">link</a>"
                 , "<img src=\"\" alt=\"image\" />"
                 , "<a href=\"#foobar\">link</a>"
                 , "<img src=\"extra/baz.png\" alt=\"image\" />"
                 , "<img src=\"/absolute/path.jpg\" alt=\"image\" /></p>"
                 ]
  testCase "rebase_relative_paths" (actual @?= expected)

