{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.Html
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
import           Control.Exception          (AsyncException, catch, throwIO)
import           GHC.Stack                  (currentCallStack)

data Opt =
       Help
     | Version
     | Tokenize
     | SourcePos
     | Highlight
     | PandocJSON
     deriving Eq

options :: [OptDescr Opt]
options =
  [ Option ['t'] ["tokenize"] (NoArg Tokenize) "tokenize"
  , Option ['p'] ["sourcepos"] (NoArg SourcePos) "source positions"
  , Option [] ["highlight"] (NoArg Highlight) "highlight"
  , Option ['j'] ["json"] (NoArg PandocJSON) "pandoc JSON output"
  , Option ['v'] ["version"] (NoArg Version) "version info"
  , Option ['h'] ["help"] (NoArg Help) "help message"
  ]

usageMessage :: String -> [OptDescr Opt] -> String
usageMessage programName = usageInfo (programName ++ " [OPTIONS] [FILES]")

main :: IO ()
main = catch (do
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
  if Highlight `elem` opts then do
      let spec = defaultSyntaxSpec
      case runWithSourceMap <$>
              runIdentity (parseCommonmarkWith spec toks) of
           Left e -> errExit e
           Right ((_ :: Html ()), sm) -> do
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
       let spec = defaultSyntaxSpec
       case runIdentity (parseCommonmarkWith spec toks) of
            Left e -> errExit e
            Right (r :: Html SourceRange)
                   -> TLIO.putStr . renderHtml $ r
    else do
      let spec = defaultSyntaxSpec
      case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Html ()) -> TLIO.putStr . renderHtml $ r)
   (\(e :: AsyncException) -> do
             currentCallStack >>= mapM_ (hPutStrLn stderr)
             throwIO e)


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
