{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Autolink
  ( autolinkSpec )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.TokParsers
import Control.Monad (guard, void)
import Text.Parsec
import Data.Text (Text)

autolinkSpec :: (Monad m, IsBlock il bl, IsInline il)
             => SyntaxSpec m il bl
autolinkSpec = mempty
  { syntaxInlineParsers = [parseAutolink]
  }

parseAutolink :: (Monad m, IsInline a) => InlineParser m a
parseAutolink = do
  void $ lookAhead $ satisfyTok $ \t ->
    case tokType t of
      WordChars -> True
      Symbol c  -> c == '.' || c == '-' || c == '_' || c == '+'
      _         -> False
  (prefix, linktext) <- withRaw $ wwwAutolink <|> urlAutolink <|> emailAutolink
  return $! link (prefix <> untokenize linktext) "" (str . untokenize $ linktext)

wwwAutolink :: Monad m => InlineParser m Text
wwwAutolink = try $ do
  lookAhead $ satisfyWord (== "www")
  validDomain
  linkSuffix
  return "http://"

validDomain :: Monad m => InlineParser m ()
validDomain = do
  let domainPart = do
        ds <- many1 $ satisfyTok (hasType WordChars)
                           <|> symbol '-'
                           <|> symbol '_'
        guard $ case reverse ds of
                     (Tok WordChars _ _ : _) -> True
                     _ -> False
  domainPart
  skipMany1 $ try (symbol '.' >> domainPart)

linkSuffix :: Monad m => InlineParser m ()
linkSuffix = try $ do
  toks <- getInput
  let possibleSuffixTok (Tok (Symbol c) _ _) =
        c `notElem` ['<','>','{','}','|','\\','^','[',']','`']
      possibleSuffixTok (Tok WordChars _ _) = True
      possibleSuffixTok _ = False
  let isDroppable (Tok (Symbol c) _ _) =
         c `elem` ['?','!','.',',',':','*','_','~']
      isDroppable _ = False
  let numToks = case dropWhile isDroppable $
                    reverse (takeWhile possibleSuffixTok toks) of
                     (Tok (Symbol ')') _ _ : xs)
                       | length [t | t@(Tok (Symbol '(') _ _) <- xs] <=
                         length [t | t@(Tok (Symbol ')') _ _) <- xs]
                       -> length xs
                     (Tok (Symbol ';') _ _
                        : Tok WordChars _ _
                        : Tok (Symbol '&') _ _
                        : xs) -> length xs
                     xs -> length xs
  count numToks anyTok
  return ()

urlAutolink :: Monad m => InlineParser m Text
urlAutolink = try $ do
  satisfyWord (`elem` ["http", "https", "ftp"])
  symbol ':'
  symbol '/'
  symbol '/'
  validDomain
  linkSuffix
  return ""

emailAutolink :: Monad m => InlineParser m Text
emailAutolink = try $ do
  let emailNameTok (Tok WordChars _ _) = True
      emailNameTok (Tok (Symbol c) _ _) =
         c == '.' || c == '-' || c == '_' || c == '+'
      emailNameTok _ = False
  skipMany1 $ satisfyTok emailNameTok
  symbol '@'
  validDomain
  return "mailto:"
