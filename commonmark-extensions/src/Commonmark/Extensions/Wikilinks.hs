{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Commonmark.Extensions.Wikilinks
  ( wikilinksSpec
  , TitlePosition(..)
  , HasWikilinks(..)
  )
where
import Commonmark.Entity
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Commonmark.Nodes
import Text.Parsec
import Data.Text (Text, strip)
import Data.Typeable (Typeable)

class HasWikilinks il where
  wikilink :: Text -> il -> il

instance Rangeable (Html a) => HasWikilinks (Html a) where
  wikilink url il =  addAttribute ("class", "wikilink") $ link url "" il

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
              (xs, [])   -> (unEntity xs, unEntity xs)
              (xs, _:ys) ->
                case titlepos of
                  TitleBeforePipe -> (unEntity xs, unEntity ys)
                  TitleAfterPipe  -> (unEntity ys, unEntity xs)
     symbol ']'
     symbol ']'
     return $ wikilink (strip url) (str (strip title))

data NodeTypeWikilinks a
  = NodeWikilink Text (Nodes a)
  deriving (Show)

instance (Typeable a, Monoid a, HasAttributes a, Rangeable a) => NodeType NodeTypeWikilinks a where
  type FromNodeType NodeTypeWikilinks a = HasWikilinks a
  fromNodeType = \case
    NodeWikilink url x -> wikilink url (fromNodes x)

instance ToPlainText (NodeTypeWikilinks a) where
  toPlainText = \case
    NodeWikilink _ x -> toPlainText x

instance (Typeable a, HasWikilinks a, Monoid a, HasAttributes a, Rangeable a) => HasWikilinks (Nodes a) where
  wikilink url x = singleNode $ NodeWikilink url x
