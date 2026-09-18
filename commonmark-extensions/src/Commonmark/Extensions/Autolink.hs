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

-- GFM: extended www and url autolinks are recognized only when
-- preceded by the beginning of a line, whitespace, or one of the
-- characters (, *, _, ~.  We additionally allow [, since
-- commonmark-hs recognizes autolinks inside square brackets
-- (see test/autolinks.md).
guardPreceded :: Monad m => InlineParser m ()
guardPreceded = do
  mbty <- getPrecedingTokType
  guard $ case mbty of
    Nothing           -> True
    Just Spaces       -> True
    Just UnicodeSpace -> True
    Just LineEnd      -> True
    Just (Symbol c)   -> c `elem` ['(', '*', '_', '~', '[']
    Just WordChars    -> False

wwwAutolink :: Monad m => InlineParser m Text
wwwAutolink = try $ do
  guardPreceded
  lookAhead $ satisfyWord (== "www")
  validDomain
  linkPath 0 0
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

linkPath :: Monad m => Int -> Int -> InlineParser m ()
linkPath openParens openBrackets = optional $ do
   Tok tt _ _ <- lookAhead anyTok
   case tt of
     Symbol '&' -> optional $
      try (symbol '&' *>
           notFollowedBy
             (try (satisfyWord (const True) *> symbol ';' *> linkEnd)) *>
           linkPath openParens openBrackets)
     Symbol '(' -> symbol '(' *> linkPath (openParens + 1) openBrackets
     Symbol ')' -> optional $ guard (openParens > 0) *> symbol ')' *> linkPath (openParens - 1) openBrackets
     Symbol '[' -> symbol '[' *> linkPath openParens (openBrackets + 1)
     Symbol ']' -> optional $ guard (openBrackets > 0) *> symbol ']' *> linkPath openParens (openBrackets - 1)
     Symbol '<' -> pure ()
     Symbol c | isTrailingPunctuation c -> optional $
         try (do skipMany1 trailingPunctuation
                 pos <- getPosition
                 linkPath openParens openBrackets
                 pos' <- getPosition
                 guard (pos' > pos)) *> linkPath openParens openBrackets
     LineEnd -> pure ()
     Spaces -> pure ()
     _ -> anyTok *> linkPath openParens openBrackets

linkEnd :: Monad m => InlineParser m ()
linkEnd = try $ skipMany trailingPunctuation *> (void whitespace <|> eof)

trailingPunctuation :: Monad m => InlineParser m Tok
trailingPunctuation =
  satisfyTok (\t -> case tokType t of
                           Symbol c -> isTrailingPunctuation c
                           _ -> False)

isTrailingPunctuation :: Char -> Bool
isTrailingPunctuation =
  (`elem` ['!', '"', '\'', ')', '*', ',', '.', ':', ';', '?', '_', '~', '<'])

urlAutolink :: Monad m => InlineParser m Text
urlAutolink = try $ do
  guardPreceded
  satisfyWord (`elem` ["http", "https", "ftp"])
  symbol ':'
  symbol '/'
  symbol '/'
  validDomain
  linkPath 0 0
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
