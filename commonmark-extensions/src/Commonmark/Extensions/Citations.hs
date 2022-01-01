{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Commonmark.Extensions.Citations
  ( HasCitations(..),
    citationsSpec
  )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap ( addName, WithSourceMap, runWithSourceMap )
import Commonmark.Html ( htmlInline, addAttribute, Html(..), ElementType(..) )
import Commonmark.TokParsers
import Control.Monad (guard)
import Data.Text (Text)
import Text.Parsec

class HasCitations il where
  citationGroup :: il -> il -- we'll parse further in pandoc
  citation :: Text -> Bool -> il
  hasCitations :: il -> Bool

instance HasCitations (Html il) where
  citationGroup =
    addAttribute ("class", "citation-group") .
    htmlInline "span" .
    Just
  citation ident suppressAuthor =
    addAttribute ("class", "citation") .
    addAttribute ("identifier", ident) .
    (if suppressAuthor
        then addAttribute ("suppress-author", "true")
      else id) $
    htmlInline "span" Nothing
  hasCitations x = case x of
    HtmlElement InlineElement "span" attr _ ->
      lookup "class" attr == Just "citation"
    HtmlConcat w v -> hasCitations w || hasCitations v
    _ -> False

instance (HasCitations il, Monoid il, Show il) => HasCitations (WithSourceMap il) where
  citationGroup x = (citationGroup <$> x) <* addName "citation"
  citation ident suppressAuthor =
    pure (citation ident suppressAuthor) <* addName "citation"
  hasCitations x =
    let (x', _) = runWithSourceMap x
     in hasCitations x'

citationsSpec
  :: forall m bl il . (Monad m , IsInline il , IsBlock il bl, HasCitations il)
  => SyntaxSpec m il bl
citationsSpec =
  defaultSyntaxSpec {
    syntaxBracketedSpecs = [citationBracketedSpec]
  , syntaxInlineParsers = [withAttributes parseBareCitation] }

 where

  citationBracketedSpec :: BracketedSpec il
  citationBracketedSpec = BracketedSpec
            { bracketedName = "Citation"
            , bracketedNests = True
            , bracketedPrefix = Nothing
            , bracketedSuffixEnd = Just ';'
            -- causes ; to be parsed in own chunk
            , bracketedSuffix = checkCitation
            }

  checkCitation _rm chunksInside = do
    -- if there are bare citations inside, return citation
    let chunkHasCitations c =
          case chunkType c of
            Parsed x -> hasCitations x
            _ -> False
    guard $ any chunkHasCitations chunksInside
    return $! citationGroup

parseBareCitation :: (Monad m, HasCitations il, IsInline il)
                  => InlineParser m il
parseBareCitation = do
  suppressAuthor <- (True <$ symbol '-') <|> pure False
  ident <- parseCitationId
  return $! citation ident suppressAuthor

parseCitationId :: Monad m => InlineParser m Text
parseCitationId = try $ do
  symbol '@'
  untokenize <$>
    (many1 (satisfyTok (\t -> case tokType t of
                 Symbol c -> c `elem` ['_']
                 WordChars -> True
                 _ -> False)))
