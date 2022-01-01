{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commonmark.Extensions.Wikilinks
  ( wikilinksSpec
  , TitlePosition(..)
  , HasWikilinks(..)
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Text.Parsec
import Data.Text (Text, strip)

class HasWikilinks il where
  wikilink :: Text -> il -> il

instance Rangeable (Html a) => HasWikilinks (Html a) where
  wikilink url il = link url "wikilink" il

instance (HasWikilinks il, Semigroup il, Monoid il)
        => HasWikilinks (WithSourceMap il) where
  wikilink url il = (wikilink url <$> il) <* addName "wikilink"

-- | Determines whether @[[foo|bar]]@ is a link to page @bar@
-- with title (description) @foo@ ('TitleBeforePipe'), as in
-- GitHub wikis, or a link to page @foo@ with title @bar@
-- ('TitleAfterPipe'), as in Obsidian and Foam.
data TitlePosition = TitleBeforePipe | TitleAfterPipe
  deriving (Show, Eq)

wikilinksSpec :: (Monad m, IsInline il, HasWikilinks il)
              => TitlePosition
              -> SyntaxSpec m il bl
wikilinksSpec titlepos = mempty
  { syntaxInlineParsers = [ pWikilink ]
  }
  where
   pWikilink = try $ do
     symbol '['
     symbol '['
     notFollowedBy (symbol '[')
     toks <- many (satisfyTok (not . hasType (Symbol ']')))
     let isPipe (Tok (Symbol '|') _ _) = True
         isPipe _ = False
     let (title, url) =
           case break isPipe toks of
              (xs, [])   -> (untokenize xs, untokenize xs)
              (xs, _:ys) ->
                case titlepos of
                  TitleBeforePipe -> (untokenize xs, untokenize ys)
                  TitleAfterPipe  -> (untokenize ys, untokenize xs)
     symbol ']'
     symbol ']'
     return $ wikilink (strip url) (str (strip title))
