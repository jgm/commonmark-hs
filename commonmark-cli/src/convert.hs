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
import           Data.Typeable
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
import           Control.Exception          (AsyncException(StackOverflow),
                                             catch, throwIO)
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
  , Option ['h'] ["highlight"] (NoArg Highlight) "highlight"
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
   (\e -> case e of
            StackOverflow -> do
             currentCallStack >>= mapM_ (hPutStrLn stderr)
             throwIO e
            _ -> throwIO e)


errExit :: ParseError -> IO a
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)

extensions :: (Monad m, Typeable m,
               Typeable bl, Typeable il,
               IsBlock il bl, IsInline il,
               HasPipeTable il bl,
               HasMath il,
               HasStrikethrough il,
               HasDefinitionList il bl,
               HasFootnote il bl)
           => [(String, SyntaxSpec m il bl)]
extensions =
  [ ("autolink", autolinkSpec)
  ,("pipe_table", pipeTableSpec)
  ,("strikethrough", strikethroughSpec)
  ,("smart", smartPunctuationSpec)
  ,("math", mathSpec)
  ,("footnote", footnoteSpec)
  ,("definition_list", definitionListSpec)
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
  HasPipeTable il bl, HasMath il,
  HasStrikethrough il,
  HasDefinitionList il bl,
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
