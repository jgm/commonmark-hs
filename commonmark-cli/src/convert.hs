{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.SourceMap
import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.Strikethrough
import           Commonmark.Extensions.PipeTable
import           Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO               as TIO
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text                  (Text)
import           Lucid
import           System.Environment
import           System.Exit
import           System.IO
import           System.Console.GetOpt
import           Paths_commonmark_cli (version)
import           Data.Version (showVersion)
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif

data Opt =
       Help
     | Version
     | Tokenize
     | SourcePos
     | Highlight
     | Extension String
     deriving Eq

options :: [OptDescr Opt]
options =
  [ Option ['t'] ["tokenize"] (NoArg Tokenize) "tokenize"
  , Option ['x'] ["extension"] (ReqArg Extension "extension") "use extension"
  , Option ['p'] ["sourcepos"] (NoArg SourcePos) "source positions"
  , Option ['h'] ["highlight"] (NoArg Highlight) "highlight"
  , Option ['v'] ["version"] (NoArg Help) "version info"
  , Option ['h'] ["help"] (NoArg Help) "help message"
  ]

usageMessage :: String -> [OptDescr Opt] -> String
usageMessage programName = usageInfo (programName ++ " [OPTIONS] [FILES]")

main :: IO ()
main = do
  (opts, files, errs) <- getOpt Permute options <$> getArgs
  when (not (null errs)) $ do
    mapM_ (hPutStrLn stderr) errs
    exitWith (ExitFailure 1)
  prg <- getProgName
  when (Help `elem` opts) $ do
    putStr (usageMessage prg options)
    exitSuccess
  when (Version `elem` opts) $ do
    putStrLn $ prg ++ " " ++ showVersion version
    exitSuccess
  toks <- if null files
            then tokenize "stdin" <$> TIO.getContents
            else mconcat <$> mapM (\f -> tokenize f <$> TIO.readFile f) files
  when (Tokenize `elem` opts) $ do
    print toks
    exitSuccess
  let parser xs = do
       let extFromName "pipe_table" = return pipeTableSpec
           extFromName "strikethrough" = return strikethroughSpec
           extFromName "smart" = return smartPunctuationSpec
           extFromName extname = do
             hPutStrLn stderr $ "Unknown extension " ++ extname
             exitWith (ExitFailure 1)
       let extensions' = [x | Extension x <- opts]
       extensions <- mconcat <$> mapM extFromName extensions'
       parseCommonmarkWith (defaultSyntaxSpec <> extensions) xs
  if Highlight `elem` opts then do
      res <- parser toks
      case runWithSourceMap <$> res of
           Left e -> errExit e
           Right ((_ :: Html ()), sm) -> do
             BL.putStr $ renderBS $ do
               doctypehtml_ $ do
                 head_ $ do
                   title_ $ case files of
                                 (x:_) -> toHtml x
                                 _     -> toHtml ("stdin" :: Text)
                   styles
                 body_ $ do
                   highlightWith sm toks
               "\n"
  else
    if SourcePos `elem` opts then do
       res <- parser toks
       case res of
            Left e -> errExit e
            Right r -> BL.putStr . renderBS . unRangedHtml $ r
    else do
       res <- parser toks
       case res of
            Left e -> errExit e
            Right (r :: Html ()) -> BL.putStr . renderBS $ r


errExit :: ParseError -> IO a
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)

highlightWith :: SourceMap -> [Tok] -> Html ()
highlightWith sm = pre_ . mconcat  . map (renderTok sm)

renderTok :: SourceMap -> Tok -> Html ()
renderTok (SourceMap sm) (Tok _ pos t) =
  case M.lookup pos sm of
       Nothing -> toHtml t
       Just (starts, ends) ->
         foldMap toEnd ends <> foldMap toStart starts <> toHtml t
    where toStart x = toHtmlRaw ("<span class=\"" <> x <> "\"" <>
                                    (if x /= "str"
                                        then "title=\"" <> x <> "\""
                                        else "") <> ">" :: Text)
          toEnd   _ = toHtmlRaw ("</span>" :: Text)

styles :: Html ()
styles = style_ $ T.unlines
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
