{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.Extensions
import           Commonmark.Pandoc
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Lazy       as BL
import qualified Text.Pandoc.Builder        as B
import           Control.Monad
import           Control.Monad.Identity
import           Data.Typeable
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.IO          as TLIO
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder     (Builder, toLazyText, fromText)
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
            then tokenize "stdin" <$> TIO.getContents
            else mconcat <$> mapM (\f -> tokenize f <$> TIO.readFile f) files
  when (Tokenize `elem` opts) $ do
    print toks
    exitSuccess
  if Highlight `elem` opts then do
      spec <- specFromExtensionNames [x | Extension x <- opts]
      case runWithSourceMap <$>
              runIdentity (parseCommonmarkWith spec toks) of
           Left e -> errExit e
           Right ((_ :: Html ()), sm) -> do
             TLIO.putStr $
               "<!DOCTYPE html>\n<head>\n" <>
               "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1.0\">\n" <>
              "<meta charset=\"utf-8\">" <>
               "<title>" <> (case files of
                                 (x:_) -> TL.pack x
                                 _     -> "stdin") <> "</title>\n" <>
               toLazyText styles <>
               "</head>\n" <>
               "<body>\n" <>
               renderHtml (highlightWith sm toks) <>
               "</body>\n"
  else
    if SourcePos `elem` opts then do
       spec <- specFromExtensionNames [x | Extension x <- opts]
       case runIdentity (parseCommonmarkWith spec toks) of
            Left e -> errExit e
            Right (r :: Html SourceRange)
                   -> TLIO.putStr . renderHtml $ r
    else
      if PandocJSON `elem` opts then do
        spec <- specFromExtensionNames [x | Extension x <- opts]
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Cm () B.Blocks) -> do
               BL.putStr . encode $ B.doc $ unCm r
               BL.putStr "\n"
      else do
        spec <- specFromExtensionNames [x | Extension x <- opts]
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

extensions :: (Monad m, Typeable m,
               Typeable bl, Typeable il,
               IsBlock il bl, IsInline il,
               HasPipeTable il bl,
               HasMath il,
               HasEmoji il,
               HasSpan il,
               ToPlainText il,
               HasStrikethrough il,
               HasSuperscript il,
               HasSubscript il,
               HasDefinitionList il bl,
               HasDiv bl,
               HasFootnote il bl)
           => [(String, SyntaxSpec m il bl)]
extensions =
  [ ("autolinks", autolinkSpec)
  ,("pipe_tables", pipeTableSpec)
  ,("strikethrough", strikethroughSpec)
  ,("superscript", superscriptSpec)
  ,("subscript", subscriptSpec)
  ,("smart", smartPunctuationSpec)
  ,("math", mathSpec)
  ,("emoji", emojiSpec)
  ,("footnotes", footnoteSpec)
  ,("definition_lists", definitionListSpec)
  ,("fancy_lists", fancyListSpec)
  ,("attributes", attributesSpec)
  ,("raw_attribute", rawAttributeSpec)
  ,("bracketed_spans", bracketedSpanSpec)
  ,("fenced_divs", fencedDivSpec)
  ,("auto_identifiers", autoIdentifiersSpec)
  ,("implicit_heading_references", implicitHeadingReferencesSpec)
  ]

extensionList :: [String]
extensionList = map fst
  (extensions :: [(String, SyntaxSpec IO (Html ()) (Html ()))])

listExtensions :: IO ()
listExtensions =
  putStr $ "Available extensions (or 'all' for all):\n" ++
    unlines (map ("  " ++) extensionList)

specFromExtensionNames ::
 (Monad m, Typeable m, Typeable bl, Typeable il,
  IsBlock il bl, IsInline il,
  HasPipeTable il bl, HasMath il, HasEmoji il,
  HasSpan il,
  ToPlainText il,
  HasStrikethrough il,
  HasSuperscript il,
  HasSubscript il,
  HasDefinitionList il bl,
  HasDiv bl,
  HasFootnote il bl)
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

highlightWith :: SourceMap -> [Tok] -> Html ()
highlightWith sm ts =
  htmlBlock "pre" $ Just $ mconcat (map (renderTok sm) ts)

renderTok :: SourceMap -> Tok -> Html ()
renderTok (SourceMap sm) (Tok _ pos t) =
  case M.lookup pos sm of
       Nothing -> htmlText t
       Just (starts, ends) ->
         foldMap toEnd ends <> foldMap toStart starts <> htmlText t
    where toStart x = htmlRaw $
                      "<span class=\"" <> x <> "\"" <>
                          (if x /= "str"
                              then " title=\"" <> x <> "\""
                              else "") <>
                          ">"
          toEnd   _ = htmlRaw "</span>"

styles :: Builder
styles = "<style>\n" <> fromText (T.unlines
  [ ".code { color: black; background-color: #eeeeee; }"
  , ".str { }"
  , ".emph { font-style: italic; }"
  , ".strong { font-weight: bold; }"
  , ".link .str { text-decoration: underline; color: magenta; }"
  , ".image .str { text-decoration: underline; color: blue; }"
  , ".heading1 { font-weight: bold; color: purple; }"
  , ".heading2 { font-weight: bold; color: purple; }"
  , ".heading3 { font-weight: bold; color: purple; }"
  , ".heading4 { font-weight: bold; color: purple; }"
  , ".heading5 { font-weight: bold; color: purple; }"
  , ".codeBlock { color: black; background-color: #eeeeee; }"
  , ".rawInline { color: coral; }"
  , ".rawBlock { color: coral; }"
  , ".escapedChar { color: gray; }"
  , ".entity { color: gray; }"
  ]
  ) <> "</style>\n"
