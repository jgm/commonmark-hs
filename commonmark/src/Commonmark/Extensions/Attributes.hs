{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Commonmark.Extensions.Attributes
  ( attributesSpec
  , HasSpan(..)
  , bracketedSpanSpec
  , rawAttributeSpec
  , pAttributes
  )
where
import Commonmark.Types
import Commonmark.Tag (htmlAttributeName, htmlDoubleQuotedAttributeValue)
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.Util
import Commonmark.SourceMap
import Commonmark.Blocks
import Commonmark.Entity (unEntity)
import Commonmark.Html
import Data.Dynamic
import qualified Data.Text as T
import Data.Tree
import Control.Monad (mzero)
import Text.Parsec

bracketedSpanSpec
             :: (Monad m, IsInline il, HasSpan il)
             => SyntaxSpec m il bl
bracketedSpanSpec = mempty
  { syntaxBracketedSpecs = [ bsSpec ]
  }
  where
   bsSpec = BracketedSpec
            { bracketedName = "Span"
            , bracketedNests = True
            , bracketedPrefix = Nothing
            , bracketedSuffixEnd = Nothing
            , bracketedSuffix = pSpanSuffix
            }
   pSpanSuffix _rm _key = do
     attrs <- pAttributes
     return $ spanWith attrs

class IsInline a => HasSpan a where
  spanWith :: Attributes -> a -> a

instance Rangeable (Html a) => HasSpan (Html a) where
  spanWith attrs ils = addAttributes attrs $ htmlInline "span" (Just ils)

instance (HasSpan i, Monoid i)
        => HasSpan (WithSourceMap i) where
  spanWith attrs x = (spanWith attrs <$> x) <* addName "span"

pRawSpan :: (IsInline a, Monad m) => InlineParser m a
pRawSpan = do
  pBacktickSpan >>=
   \case
    Left ticks     -> return $ str (untokenize ticks)
    Right codetoks -> do
      let raw = untokenize codetoks
      (do f <- pRawAttribute
          return $ rawInline f raw)
       <|> return (code . normalizeCodeSpan $ raw)

rawAttributeSpec :: (Monad m, IsBlock il bl)
                         => SyntaxSpec m il bl
rawAttributeSpec = mempty
  { syntaxBlockSpecs = [ ] -- TODO rawAttributeBlockSpec
  , syntaxInlineParsers = [ pRawSpan ]
  }

rawAttributeBlockSpec :: (Monad m, IsBlock il bl)
                              => BlockSpec m il bl
rawAttributeBlockSpec = undefined

-- | Allow attributes on everything.
attributesSpec
             :: (Monad m, IsInline il)
             => SyntaxSpec m il bl
attributesSpec = mempty
  { syntaxAttributeParsers = [pAttributes]
  }

pAttributes :: Monad m => ParsecT [Tok] u m Attributes
pAttributes = collapseAttributes . mconcat <$> many1 pattr
  where
    pattr = try $ do
      symbol '{'
      optional whitespace
      let pAttribute = pIdentifier <|> pClass <|> pKeyValue
      a <- pAttribute
      as <- many $ try (whitespace *> (pIdentifier <|> pClass <|> pKeyValue))
      optional whitespace
      symbol '}'
      return (a:as)

-- | Ensure that attributes contain only one 'class' and one 'id'.
-- Concatenate the classes with spaces between, if multiple.
collapseAttributes :: Attributes -> Attributes
collapseAttributes xs =
  let classes = [y | ("class", y) <- xs] in
 (case lookup "id" xs of
     Just id' -> (("id",id'):)
     Nothing  -> id) .
  (if null classes
      then id
      else (("class", T.unwords classes):)) $
  [(k,v) | (k,v) <- xs, k /= "id" && k /= "class"]

pRawAttribute :: Monad m => ParsecT [Tok] u m Format
pRawAttribute = try $ do
  symbol '{'
  optional whitespace
  symbol '='
  Tok _ _ t <- satisfyWord (const True)
  optional whitespace
  symbol '}'
  return $ Format t

pIdentifier :: Monad m => ParsecT [Tok] u m Attribute
pIdentifier = try $ do
  symbol '#'
  xs <- many1 $
        satisfyWord (const True)
    <|> satisfyTok (\c -> hasType (Symbol '-') c || hasType (Symbol '_') c
                        || hasType (Symbol ':') c || hasType (Symbol '.') c)
  return ("id", unEntity xs)

pClass :: Monad m => ParsecT [Tok] u m Attribute
pClass = do
  symbol '.'
  xs <- many1 $
        satisfyWord (const True)
    <|> satisfyTok (\c -> hasType (Symbol '-') c || hasType (Symbol '_') c)
  return ("class", unEntity xs)

pKeyValue :: Monad m => ParsecT [Tok] u m Attribute
pKeyValue = do
  name <- htmlAttributeName
  symbol '='
  val <- htmlDoubleQuotedAttributeValue
       <|> many1 (noneOfToks [Spaces, LineEnd, Symbol '<', Symbol '>',
                      Symbol '=', Symbol '`', Symbol '\'', Symbol '"',
                      Symbol '}'])
  let val' = case val of
               Tok (Symbol '"') _ _:_:_  -> drop 1 $ init $ val
               Tok (Symbol '\'') _ _:_:_ -> mzero
               _ -> val
  return (untokenize name, unEntity val')

