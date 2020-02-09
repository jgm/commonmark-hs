{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.Html
-- import           Commonmark.Pandoc
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy       as BL
-- import qualified Text.Pandoc.Builder        as B
import           Control.Monad
import           Control.Monad.Identity
import           Data.Typeable
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy           as BL
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
     | ListExtensions
     | Extension String
     deriving Eq

options :: [OptDescr Opt]
options =
  [ Option ['t'] ["tokenize"] (NoArg Tokenize) "tokenize"
  , Option ['x'] ["extension"] (ReqArg Extension "extension") "use extension"
  , Option ['p'] ["sourcepos"] (NoArg SourcePos) "source positions"
  , Option [] ["highlight"] (NoArg Highlight) "highlight"
  , Option ['j'] ["json"] (NoArg PandocJSON) "pandoc JSON output"
  , Option ['v'] ["version"] (NoArg Version) "version info"
  , Option [] ["list-extensions"] (NoArg ListExtensions) "list extensions"
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
  when (ListExtensions `elem` opts) $ do
    listExtensions
    exitSuccess
  when (Version `elem` opts) $ do
    putStrLn $ prg ++ " " ++ showVersion version
    exitSuccess
  toks <- if null files
            then tokenize "stdin" <$> B.getContents
            else mconcat <$> mapM (\f -> tokenize f <$> B.readFile f) files
  when (Tokenize `elem` opts) $ do
    print toks
    exitSuccess
  if Highlight `elem` opts then do
      spec <- specFromExtensionNames [x | Extension x <- opts]
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
       spec <- specFromExtensionNames [x | Extension x <- opts]
       case runIdentity (parseCommonmarkWith spec toks) of
            Left e -> errExit e
            Right (r :: Html SourceRange)
                   -> BL.putStr . renderHtml $ r
    else
      do
        spec <- specFromExtensionNames [x | Extension x <- opts]
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Html ()) -> BL.putStr . renderHtml $ r)
   (\(e :: AsyncException) -> do
             currentCallStack >>= mapM_ (hPutStrLn stderr)
             throwIO e)


errExit :: ParseError -> IO a
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)

extensions :: (Monad m, Typeable m,
               Typeable bl, Typeable il,
               IsBlock il bl, IsInline il)
--               HasPipeTable il bl,
--               HasMath il,
--               HasEmoji il,
--               HasSpan il,
--               ToPlainText il,
--               HasStrikethrough il,
--               HasSuperscript il,
--               HasSubscript il,
--               HasDefinitionList il bl,
--               HasDiv bl,
--               HasFootnote il bl)
           => [(String, SyntaxSpec m il bl)]
extensions = []
--  [ ("autolinks", autolinkSpec)
--  ,("pipe_tables", pipeTableSpec)
--  ,("strikethrough", strikethroughSpec)
--  ,("superscript", superscriptSpec)
--  ,("subscript", subscriptSpec)
--  ,("smart", smartPunctuationSpec)
--  ,("math", mathSpec)
--  ,("emoji", emojiSpec)
--  ,("footnotes", footnoteSpec)
--  ,("definition_lists", definitionListSpec)
--  ,("fancy_lists", fancyListSpec)
--  ,("attributes", attributesSpec)
--  ,("raw_attribute", rawAttributeSpec)
--  ,("bracketed_spans", bracketedSpanSpec)
--  ,("fenced_divs", fencedDivSpec)
--  ,("auto_identifiers", autoIdentifiersSpec)
--  ,("implicit_heading_references", implicitHeadingReferencesSpec)
--  ]

extensionList :: [String]
extensionList = map fst
  (extensions :: [(String, SyntaxSpec IO (Html ()) (Html ()))])

listExtensions :: IO ()
listExtensions =
  putStr $ "Available extensions (or 'all' for all):\n" ++
    unlines (map ("  " ++) extensionList)

specFromExtensionNames ::
 (Monad m, Typeable m, Typeable bl, Typeable il,
  IsBlock il bl, IsInline il)
  -- HasPipeTable il bl, HasMath il, HasEmoji il,
  -- HasSpan il,
  -- ToPlainText il,
  -- HasStrikethrough il,
  -- HasSuperscript il,
  -- HasSubscript il,
  -- HasDefinitionList il bl,
  -- HasDiv bl,
  -- HasFootnote il bl)
  => [String] -> IO (SyntaxSpec m il bl)
specFromExtensionNames extnames = do
 let extFromName name =
       case lookup name extensions of
         Just ext -> return ext
         Nothing  -> do
           hPutStrLn stderr $ "Unknown extension " ++ name
           listExtensions
           exitWith (ExitFailure 1)
 exts <- if "all" `elem` extnames
            then return $ mconcat (map snd extensions)
            else mconcat <$> mapM extFromName extnames
 return $ exts <> defaultSyntaxSpec

highlightWith :: SourceMap -> [Tok] -> Builder
highlightWith sm ts = "<pre>" <>  mconcat (map (renderTok sm) ts) <> "</pre>"

renderTok :: SourceMap -> Tok -> Builder
renderTok (SourceMap sm) (Tok _ pos t) =
  case M.lookup pos sm of
       Nothing -> "SntOUe"-- fromText t
       Just (starts, ends) ->
         foldMap toEnd ends <> foldMap toStart starts <> "SNOTHU"
    where toStart x = "<span class=\"" <> "SNTHOU" <> "\"" <>
                          (if x /= "str"
                              then "title=\"" <> "sntoeu" <> "\""
                              else "") <>
                          ">"
          toEnd   _ = "</span>"

styles :: Builder
styles = "<style>\n" 
