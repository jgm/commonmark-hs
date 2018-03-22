{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
import           Commonmark
import           Commonmark.SourceMap
import           Control.Monad
import qualified Data.ByteString.Lazy as BL
import           Data.List            (partition)
import qualified Data.Map             as M
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Lucid                as L
import           System.Environment
import           System.Exit
import           System.IO
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

main :: IO ()
main = do
  args <- getArgs
  let isopt ('-':'-':_) = True
      isopt _           = False
  let (opts, files) = partition isopt args
  when ("--help" `elem` opts) $ do
    putStrLn "commonmark-highlight [FILE..]"
    putStrLn "--help             this message"
    exitSuccess
  toks <- if null files
            then tokenize "stdin" <$> TIO.getContents
            else mconcat <$> mapM (\f -> tokenize f <$> TIO.readFile f) files
  case parseCommonmark toks of
        Left e -> errExit e
        Right (res :: WithSourceMap (L.Html ())) -> do
          let (_, sm) = runWithSourceMap res
          BL.putStr $ L.renderBS styles
          BL.putStr $ L.renderBS $ highlightWith sm toks
          BL.putStr "\n"

highlightWith :: SourceMap -> [Tok] -> L.Html ()
highlightWith sm = L.pre_ . mconcat  . map (renderTok sm)

renderTok :: SourceMap -> Tok -> L.Html ()
renderTok (SourceMap sm) (Tok _ pos t) =
  case M.lookup pos sm of
       Nothing -> L.toHtml t
       Just (starts, ends) ->
         foldMap toEnd ends <> foldMap toStart starts <> L.toHtml t
    where toStart x = L.toHtmlRaw ("<span class=\"" <> x <> "\"" <>
                                    (if x /= "str"
                                        then "title=\"" <> x <> "\""
                                        else "") <> ">" :: Text)
          toEnd   _ = L.toHtmlRaw ("</span>" :: Text)

styles :: L.Html ()
styles = L.style_ $ T.unlines
  [ "pre { color: silver; }"
  , "span.str { color: black; }"
  , "span.code { color: teal; }"
  , "span.emph { font-style: italic; }"
  , "span.strong { font-weight: bold; }"
  , "span.link span.str { text-decoration: underline; color: magenta; }"
  , "span.image span.str { text-decoration: underline; color: blue; }"
  , "span.header1 span.str { font-weight: bold; color: purple; }"
  , "span.header2 span.str { font-weight: bold; color: purple; }"
  , "span.header3 span.str { font-weight: bold; color: purple; }"
  , "span.header4 span.str { font-weight: bold; color: purple; }"
  , "span.header5 span.str { font-weight: bold; color: purple; }"
  , "span.codeBlock { color: teal; }"
  , "span.rawInline { color: gray; }"
  , "span.rawBlock { color: gray; }"
  , "span.escapedChar { color: gray; }"
  , "span.entity { color: gray; }"
  ]

errExit :: ParseError -> IO ()
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)
