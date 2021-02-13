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
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.TokParsers (symbol)
import Commonmark.Html
import Text.Parsec
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
import Data.Text (Text)
import qualified Data.Text as T

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
  { syntaxBracketedSpecs = [ wlspec ]
  }
  where
   wlspec = BracketedSpec
            { bracketedName = "Wikilink"
            , bracketedNests = True
            , bracketedPrefix = Just '['
            , bracketedSuffixEnd = Just ']'
            , bracketedSuffix = pWikilinkSuffix
            }
   pWikilinkSuffix _rm contents = try $ do
     symbol ']'
     let toks = concat $ map fst contents -- FIXME
     let isPipe (Tok (Symbol '|') _ _) = True
         isPipe _ = False
     let (mbtitle, pageOrUrl) = case break isPipe toks of
                                   (xs, _:ys) -> (Just xs, ys)
                                   (xs, [])   -> (Nothing, xs)
     let dest = untokenize pageOrUrl
     let isUrl = "://" `T.isInfixOf` dest -- TODO something better
     let description = maybe (str dest) (str . untokenize) mbtitle
     return $ \_ ->  -- we ignore the inlines passed in...
       if isUrl
          then link dest mempty description
          else wikilink dest description
