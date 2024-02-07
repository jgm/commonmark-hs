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
  linkPath 0
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

linkPath :: Monad m => Int -> InlineParser m ()
linkPath openParens =
      try (symbol '&' *>
           notFollowedBy
             (try (satisfyWord (const True) *> symbol ';' *> linkEnd)) *>
           linkPath openParens)
  <|> (pathPunctuation *> linkPath openParens)
  <|> (symbol '(' *> linkPath (openParens + 1))
  <|> (guard (openParens > 0) *> symbol ')' *> linkPath (openParens - 1))
  -- the following clause is needed to implement the GFM spec, which allows
  -- unbalanced ) except at link end. However, leaving this in causes
  -- problematic interaction with explicit link syntax in certain odd cases (see #147).
  -- <|> (notFollowedBy linkEnd *> symbol ')' *> linkPath (openParens - 1))
  <|> (satisfyTok (\t -> case tokType t of
                            LineEnd -> False
                            Spaces -> False
                            Symbol c -> not (isTrailingPunctuation c || c == '&' || c == ')')
                            _ -> True) *> linkPath openParens)
  <|> pure ()

linkEnd :: Monad m => InlineParser m ()
linkEnd = try $ skipMany trailingPunctuation *> (void whitespace <|> eof)

trailingPunctuation :: Monad m => InlineParser m ()
trailingPunctuation = void $
  satisfyTok (\t -> case tokType t of
                           Symbol c -> isTrailingPunctuation c
                           _ -> False)

isTrailingPunctuation :: Char -> Bool
isTrailingPunctuation =
  (`elem` ['!', '"', '\'', ')', '*', ',', '.', ':', ';', '?', '_', '~', '<'])

pathPunctuation :: Monad m => InlineParser m ()
pathPunctuation = try $ do
  satisfyTok (\t -> case tokType t of
                       Symbol c -> isTrailingPunctuation c && c /= ')' && c /= '<'
                       _        -> False)
  void $ lookAhead (satisfyTok (\t -> case tokType t of
                                        WordChars -> True
                                        _ -> False))

urlAutolink :: Monad m => InlineParser m Text
urlAutolink = try $ do
  satisfyWord (`elem` ["http", "https", "ftp"])
  symbol ':'
  symbol '/'
  symbol '/'
  validDomain
  linkPath 0
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
