{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.Extensions
import           Commonmark.Pandoc
import           Data.Maybe                 (isJust)
import           Data.Aeson                 (encode)
import qualified Data.Sequence              as Seq
import qualified Data.ByteString.Lazy       as BL
import qualified Text.Pandoc.Builder        as B
import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Typeable
import qualified Data.Text.IO               as TIO
import qualified Data.Text.Lazy.IO          as TLIO
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           System.Environment
import           System.Exit
import           System.IO
import           System.Console.GetOpt
import           Paths_commonmark_cli (version)
import           Data.Version (showVersion)
import           Control.Exception          (AsyncException, catch, throwIO)
import           GHC.Stack                  (currentCallStack)
import           System.Console.ANSI

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
  , Option [] ["highlight"] (NoArg Highlight) "highlight using ANSI"
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
  unless (null errs) $ do
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
           Right (_ :: Html (), sm) -> highlightWith sm toks
  else do
    let sourcepos = SourcePos `elem` opts
    let json = PandocJSON `elem` opts
    case (json, sourcepos) of
      (True, True) -> do
        spec <- specFromExtensionNames [x | Extension x <- opts]
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Cm SourceRange B.Blocks) -> do
               BL.putStr . encode $ B.doc $ unCm r
               BL.putStr "\n"
      (True, False) -> do
        spec <- specFromExtensionNames [x | Extension x <- opts]
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Cm () B.Blocks) -> do
               BL.putStr . encode $ B.doc $ unCm r
               BL.putStr "\n"
      (False, True) -> do
        spec <- specFromExtensionNames [x | Extension x <- opts]
        case runIdentity (parseCommonmarkWith spec toks) of
             Left e -> errExit e
             Right (r :: Html SourceRange) -> TLIO.putStr . renderHtml $ r
      (False, False) -> do
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
               HasQuoted il,
               HasEmoji il,
               HasWikilinks il,
               HasSpan il,
               ToPlainText il,
               HasStrikethrough il,
               HasSuperscript il,
               HasSubscript il,
               HasCitations il,
               HasDefinitionList il bl,
               HasDiv bl,
               HasTaskList il bl,
               HasFootnote il bl)
           => [(String, SyntaxSpec m il bl)]
extensions =
  [ ("autolinks", autolinkSpec)
  ,("pipe_tables", pipeTableSpec)
  ,("hard_line_breaks", hardLineBreaksSpec)
  ,("strikethrough", strikethroughSpec)
  ,("superscript", superscriptSpec)
  ,("subscript", subscriptSpec)
  ,("citations", citationsSpec)
  ,("smart", smartPunctuationSpec)
  ,("math", mathSpec)
  ,("emoji", emojiSpec)
  ,("footnotes", footnoteSpec)
  ,("definition_lists", definitionListSpec)
  ,("fancy_lists", fancyListSpec)
  ,("task_lists", taskListSpec)
  ,("attributes", attributesSpec)
  ,("raw_attribute", rawAttributeSpec)
  ,("bracketed_spans", bracketedSpanSpec)
  ,("fenced_divs", fencedDivSpec)
  ,("auto_identifiers", autoIdentifiersSpec)
  ,("auto_identifiers_ascii", autoIdentifiersAsciiSpec)
  ,("implicit_heading_references", implicitHeadingReferencesSpec)
  ,("wikilinks_title_before_pipe", wikilinksSpec TitleBeforePipe)
  ,("wikilinks_title_after_pipe", wikilinksSpec TitleAfterPipe)
  ,("rebase_relative_paths", rebaseRelativePathsSpec)
  ,("gfm", gfmExtensions)
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
  HasPipeTable il bl, HasMath il, HasQuoted il, HasEmoji il,
  HasWikilinks il, HasSpan il,
  ToPlainText il,
  HasStrikethrough il,
  HasSuperscript il,
  HasSubscript il,
  HasCitations il,
  HasDefinitionList il bl,
  HasDiv bl,
  HasTaskList il bl,
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

highlightWith :: SourceMap -> [Tok] -> IO ()
highlightWith sm ts = evalStateT (mapM_ (hlTok sm) ts) (mempty, [])

hlTok :: SourceMap -> Tok
      -> StateT (Seq.Seq T.Text, [SGR]) IO ()
hlTok (SourceMap !sm) (Tok toktype !pos !t) = do
  (xs, sgrs) <- get
  -- When we encounter a line end, we store it in mbLineEnd
  -- and output it after the formatting codes at the beginning
  -- of the next line.  Otherwise the wrong lines are affected.
  case M.lookup pos sm of
       Nothing -> liftIO $ do
         when (toktype == LineEnd) $
           -- This escape sequence paints rest of line with current
           -- background color; otherwise we get bad results with scrolling.
           TIO.putStr "\27[K"
         TIO.putStr t
       Just (starts, ends) -> do
         let xsMinusEnds = foldr (\e s ->
                             case Seq.viewr s of
                               Seq.EmptyR -> s
                               z Seq.:> x
                                  | x == e -> z
                                  | otherwise -> s) xs (Seq.reverse ends)
         let xs' = xsMinusEnds <> starts
         let isStr = xs' `has` "str"
         let sgrs' = if xs == xs'
                        then sgrs
                        else Reset :
                             SetConsoleIntensity
                               (if isStr
                                   then NormalIntensity
                                   else FaintIntensity) :
                             foldMap sgrFrom xs'
         put (xs', sgrs')
         liftIO $ do
            when (sgrs /= sgrs') $ setSGR sgrs'
            when (toktype == LineEnd) $
              TIO.putStr "\27[K"
            TIO.putStr t

sgrFrom :: T.Text -> [SGR]
sgrFrom t =
  case t of
     "link"          -> [SetUnderlining SingleUnderline]
     "image"         -> [SetColor Foreground Vivid Magenta]
     "entity"        -> [SetColor Foreground Vivid Magenta]
     "escapedChar"   -> [SetColor Foreground Vivid Magenta]
     "code"          -> [SetColor Foreground Vivid White,
                         SetColor Background Dull Cyan]
     "codeBlock"     -> [SetColor Foreground Vivid White,
                         SetColor Background Dull Cyan]
     "rawInline"     -> [SetColor Foreground Vivid Green]
     "rawBlock"      -> [SetColor Foreground Vivid Green]
     "heading1"      -> [SetColor Foreground Vivid Blue]
     "heading2"      -> [SetColor Foreground Vivid Blue]
     "heading3"      -> [SetColor Foreground Vivid Blue]
     "heading4"      -> [SetColor Foreground Vivid Blue]
     "heading5"      -> [SetColor Foreground Vivid Blue]
     "heading6"      -> [SetColor Foreground Vivid Blue]
     "blockQuote"    -> [SetColor Foreground Dull Cyan]
     "referenceLinkDefinition"
                     -> [SetColor Foreground Dull Cyan,
                         SetUnderlining SingleUnderline]
     "emoji"         -> [SetColor Foreground Vivid Magenta]
     "math"          -> [SetColor Foreground Vivid Magenta]
     "strikethrough" -> [SetColor Foreground Vivid Magenta]
     "superscript"   -> [SetColor Foreground Vivid Magenta]
     "subscript"     -> [SetColor Foreground Vivid Magenta]
     "footnote"      -> [SetColor Foreground Vivid Magenta]
     "footnoteRef"   -> [SetColor Foreground Vivid Magenta]
     _               -> []

has :: Seq.Seq T.Text -> T.Text -> Bool
has xs t = isJust $ t `Seq.elemIndexL` xs
