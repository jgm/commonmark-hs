{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.Pandoc
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy       as BL
import qualified Text.Pandoc.Builder        as B
import           Control.Monad
import           Control.Monad.Identity
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.IO          as TLIO
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           Data.Text.Lazy.Builder     (Builder, toLazyText,
                                             fromText, fromString)
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
     | PandocJSON
     | Extension String
     deriving Eq

options :: [OptDescr Opt]
options =
  [ Option ['t'] ["tokenize"] (NoArg Tokenize) "tokenize"
  , Option ['x'] ["extension"] (ReqArg Extension "extension") "use extension"
  , Option ['p'] ["sourcepos"] (NoArg SourcePos) "source positions"
  , Option ['h'] ["highlight"] (NoArg Highlight) "highlight"
  , Option ['j'] ["json"] (NoArg PandocJSON) "pandoc JSON output"
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
  let extFromName "pipe_table" = return pipeTableSpec
      extFromName "strikethrough" = return strikethroughSpec
      extFromName "smart" = return smartPunctuationSpec
      extFromName "math" = return mathSpec
      extFromName "autolink" = return autolinkSpec
      extFromName "footnote" = return footnoteSpec
      extFromName extname = do
        hPutStrLn stderr $ "Unknown extension " ++ extname
        exitWith (ExitFailure 1)
  if Highlight `elem` opts then do
      extensions <- mconcat <$> mapM extFromName [x | Extension x <- opts]
      let spec = extensions <> defaultSyntaxSpec
      case runWithSourceMap <$>
              runIdentity (parseCommonmarkWith spec toks) of
           Left e -> errExit e
           Right ((_ :: Builder), sm) -> do
             TLIO.putStr $ toLazyText $
               "<!DOCTYPE html>\n<head>\n" <>
               "<title>" <> (case files of
                                 (x:_) -> fromString x
                                 _     -> "stdin") <> "</title>\n" <>
               styles <>
               "</head>\n" <>
               "<body>\n" <>
               highlightWith sm toks <>
               "</body>\n"
  else
    if SourcePos `elem` opts then do
       extensions <- mconcat <$> mapM extFromName [x | Extension x <- opts]
       let spec = extensions <> defaultSyntaxSpec
       case runIdentity (parseCommonmarkWith spec toks) of
            Left e -> errExit e
            Right r -> TLIO.putStr . toLazyText $ r
    else
      if PandocJSON `elem` opts then do
        extensions <- mconcat <$> mapM extFromName [x | Extension x <- opts]
        let spec = extensions <> defaultSyntaxSpec
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Cm () B.Blocks) -> do
               BL.putStr . encode $ B.doc $ unCm r
               BL.putStr "\n"
      else do
        extensions <- mconcat <$> mapM extFromName [x | Extension x <- opts]
        let spec = extensions <> defaultSyntaxSpec
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Builder) -> TLIO.putStr . toLazyText $ r

errExit :: ParseError -> IO a
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)

highlightWith :: SourceMap -> [Tok] -> Builder
highlightWith sm ts = "<pre>" <>  mconcat (map (renderTok sm) ts) <> "</pre>"

renderTok :: SourceMap -> Tok -> Builder
renderTok (SourceMap sm) (Tok _ pos t) =
  case M.lookup pos sm of
       Nothing -> fromText t
       Just (starts, ends) ->
         foldMap toEnd ends <> foldMap toStart starts <> fromText t
    where toStart x = "<span class=\"" <> fromText x <> "\"" <>
                          (if x /= "str"
                              then "title=\"" <> fromText x <> "\""
                              else "") <>
                          ">"
          toEnd   _ = "</span>"

styles :: Builder
styles = "<style>\n" <> fromText (T.unlines
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
  ]) <> "</style>\n"
