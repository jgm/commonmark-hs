{-# LANGUAGE CPP                 #-}
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
#if !MIN_VERSION_base(4,11,0)
import           Data.Monoid
#endif
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
           Right ((_ :: Html ()), sm) -> highlightWith sm toks
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

highlightWith :: SourceMap -> [Tok] -> IO ()
highlightWith sm ts = evalStateT (mapM_ (hlTok sm) ts) mempty

hlTok :: SourceMap -> Tok -> StateT (Seq.Seq T.Text) IO ()
hlTok (SourceMap sm) (Tok toktype pos t) = do
  xs <- get
  case M.lookup pos sm of
       Nothing -> liftIO $ do
         if toktype == LineEnd
            then do
              setSGR []
              TIO.putStr t
              setSGR (sgrFrom xs)
            else TIO.putStr t
       Just (starts, ends) -> do
         let xsMinusEnds = foldr (\e s ->
                             case Seq.viewr s of
                               Seq.EmptyR -> s
                               z Seq.:> x
                                  | x == e -> z
                                  | otherwise -> s) xs (Seq.reverse ends)
         let xs' = xsMinusEnds <> starts
         put xs'
         liftIO $
            if toktype == LineEnd
               then do
                 setSGR []
                 TIO.putStr t
                 setSGR (sgrFrom xs')
               else if xs == xs'
                 then TIO.putStr t
                 else do
                   setSGR []
                   setSGR (sgrFrom xs')
                   TIO.putStr t

sgrFrom :: Seq.Seq T.Text -> [SGR]
sgrFrom xs =
  (if xs `has` "link"
      then (SetUnderlining SingleUnderline :)
      else id) $
  case () of
   _ | xs `has` "str" ->
          SetColor Foreground Vivid Black : normalSGRs
     | xs `has` "entity" ->
          SetColor Foreground Vivid Magenta : normalSGRs
     | xs `has` "escapedChar" ->
          SetColor Foreground Vivid Magenta : normalSGRs
     | xs `has` "code" || xs `has` "codeBlock" ->
          [SetColor Foreground Vivid White,
           SetColor Background Dull Cyan]
     | xs `has` "rawInline" || xs `has` "rawBlock" ->
          [SetColor Foreground Vivid Green]
     | otherwise -> [SetColor Foreground Dull Cyan]
   where normalSGRs =
          [SetConsoleIntensity BoldIntensity | xs `has` "strong"] <>
          [SetItalicized True | xs `has` "emph"] <>
          [SetColor Foreground Vivid Magenta | xs `has` "image"] <>
          [SetColor Foreground Vivid Blue |
             xs `has` "heading1" || xs `has` "heading2" ||
             xs `has` "heading3" || xs `has` "heading4" ||
             xs `has` "heading5" || xs `has` "heading6"] <>
          [SetColor Foreground Dull Cyan | xs `has` "blockQuote"]


has :: Seq.Seq T.Text -> T.Text -> Bool
has xs t = isJust $ t `Seq.elemIndexL` xs
