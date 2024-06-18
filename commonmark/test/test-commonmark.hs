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

normalizeHtml :: Text -> Text
normalizeHtml = T.replace "\n</li>" "</li>" .
                T.replace "<li>\n" "<li>"

fromRight :: b -> Either a b ->  b
fromRight fallback (Left _) = fallback
fromRight _ (Right x)       = x

tokenize_roundtrip :: String -> Bool
tokenize_roundtrip s = untokenize (tokenize "source" t) == t
  where t = normalize NFC $ T.pack s

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
