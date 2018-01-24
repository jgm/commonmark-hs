{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Commonmark
import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.Strikethrough
import           Commonmark.Extensions.PipeTable
import           Control.Monad
import           Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO               as TIO
import           Lucid
import           System.Environment
import           System.Exit
import           System.IO
import           Data.Functor.Identity      (runIdentity)
import           System.Console.GetOpt
import           Paths_commonmark (version)
import           Data.Version (showVersion)

data Opt =
       Help
     | Version
     | Tokenize
     | SourcePos
     | Extension String
     deriving Eq

options :: [OptDescr Opt]
options =
  [ Option ['t'] ["tokenize"] (NoArg Tokenize) "tokenize"
  , Option ['x'] ["extension"] (ReqArg Extension "extension") "use extension"
  , Option ['p'] ["sourcepos"] (NoArg SourcePos) "source positions"
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
  if SourcePos `elem` opts then do
     res <- parser toks
     case res of
          Left e -> errExit e
          Right r -> BL.putStr . renderBS . unRangedHtml $ r
  else do
     res <- parser toks
     case res of
          Left e -> errExit e
          Right r -> BL.putStr . renderBS $ r

errExit :: ParseError -> IO a
errExit err = do
  hPrint stderr err
  exitWith (ExitFailure 1)
