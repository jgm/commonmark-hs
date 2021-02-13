{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Commonmark.Extensions.Wikilinks
  ( wikilinksSpec
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
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text (Text)

class HasWikilinks il where
  wikilink :: Text -> il -> il

instance Rangeable (Html a) => HasWikilinks (Html a) where
  wikilink url il = link url "wikilink" il

instance (HasWikilinks il, Semigroup il, Monoid il)
        => HasWikilinks (WithSourceMap il) where
  wikilink url il = (wikilink url <$> il) <* addName "wikilink"


wikilinksSpec :: (Monad m, IsInline il, HasWikilinks il)
              => SyntaxSpec m il bl
wikilinksSpec = mempty
  { syntaxInlineParsers = [ pWikilink ]
  }
  where
   pWikilink = do
     symbol '['
     symbol '['
     notFollowedBy (symbol '[')
     title <- untokenize <$>
                many (satisfyTok (\t ->
                  not (hasType (Symbol '|') t || hasType (Symbol ']') t)))
     url <- option title $ untokenize <$> (symbol '|' *>
                     many (satisfyTok (not . hasType (Symbol ']'))))
     symbol ']'
     symbol ']'
     return $ wikilink url (str title)
