{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Commonmark.Extensions.Attributes
  ( attributesSpec
  , HasDiv(..)
  , fencedDivSpec
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
import Commonmark.TokParsers
import Commonmark.SourceMap
import Commonmark.Blocks
import Commonmark.Entity (unEntity)
import Commonmark.Html
import Data.Dynamic
import Data.Tree
import Control.Monad (mzero, guard, void)
import Text.Parsec

class HasDiv bl where
  div_ :: bl -> bl

instance HasDiv (Html a) where
  div_ bs = htmlBlock "div" $ Just (htmlRaw "\n" <> bs)

instance (HasDiv bl, Semigroup bl)
        => HasDiv (WithSourceMap bl) where
  div_ bs = (div_ <$> bs) <* addName "div"

fencedDivSpec
             :: (Monad m, IsInline il, IsBlock il bl, HasDiv bl)
             => SyntaxSpec m il bl
fencedDivSpec = mempty
  { syntaxBlockSpecs = [fencedDivBlockSpec] }

fencedDivBlockSpec :: (Monad m, IsBlock il bl, HasDiv bl)
                   => BlockSpec m il bl
fencedDivBlockSpec = BlockSpec
    { blockType           = "FencedDiv"
     , blockStart          = try $ do
             prepos <- getPosition
             nonindentSpaces
             pos <- getPosition
             let indentspaces = sourceColumn pos - sourceColumn prepos
             colons <- many1 (symbol ':')
             let fencelength = length colons
             guard $ fencelength >= 3
             skipWhile (hasType Spaces)
             attrs <- pAttributes <|>
                      (do bareWordToks <- many1
                           (satisfyWord (const True) <|> anySymbol)
                          return [("class", untokenize bareWordToks)])
             skipWhile (hasType Spaces)
             lookAhead $ void lineEnd <|> eof
             addNodeToStack $
                Node (defBlockData fencedDivBlockSpec){
                          blockData = toDyn
                               (fencelength, indentspaces, attrs),
                          blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node -> try (do
             nonindentSpaces
             pos <- getPosition
             ts <- many1 (symbol ':')
             let closelength = length ts
             skipWhile (hasType Spaces)
             lookAhead $ void lineEnd <|> eof
             let fencelength = getFenceLength node
             guard $ closelength >= fencelength
             -- ensure that there aren't subordinate open fenced divs
             -- with fencelength <= closelength:
             ns <- nodeStack <$> getState
             guard $ not $ any
               (\n ->
                 (blockType (blockSpec (rootLabel n))) == "FencedDiv" &&
                 (getFenceLength n) <= closelength) $
               takeWhile (\n -> not
                    (blockType (blockSpec (rootLabel n)) == "FencedDiv" &&
                     blockStartPos (rootLabel n) ==
                     blockStartPos (rootLabel node)))
               ns
             endOfBlock
             return $! (pos, node))
               <|> (do let ((_, indentspaces, _)
                              :: (Int, Int, Attributes)) = fromDyn
                                   (blockData (rootLabel node))
                                   (3, 0, mempty)
                       pos <- getPosition
                       _ <- gobbleUpToSpaces indentspaces
                       return $! (pos, node))
     , blockConstructor    = \node -> do
           let ((_, _, attrs) :: (Int, Int, Attributes)) =
                   fromDyn (blockData (rootLabel node)) (3, 0, mempty)
           (addAttributes attrs . div_ . mconcat)
             <$> renderChildren node
     , blockFinalize       = defaultFinalizer
     }

getFenceLength :: (Monad m, IsBlock il bl, HasDiv bl)
               => BlockNode m il bl -> Int
getFenceLength node =
  let ((fencelength, _, _)
         :: (Int, Int, Attributes)) = fromDyn
                        (blockData (rootLabel node))
                        (3, 0, mempty)
  in fencelength

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
     return $! spanWith attrs

class IsInline a => HasSpan a where
  spanWith :: Attributes -> a -> a

instance Rangeable (Html a) => HasSpan (Html a) where
  spanWith attrs ils = addAttributes attrs $ htmlInline "span" (Just ils)

instance (HasSpan i, Semigroup i, Monoid i)
        => HasSpan (WithSourceMap i) where
  spanWith attrs x = (spanWith attrs <$> x) <* addName "span"

pRawSpan :: (IsInline a, Monad m) => InlineParser m a
pRawSpan = try $ do
  tok <- symbol '`'
  pBacktickSpan tok >>=
   \case
    Left ticks     -> return $! str (untokenize ticks)
    Right codetoks -> do
      let raw = untokenize codetoks
      (do f <- pRawAttribute
          return $! rawInline f raw)
       <|> (return $! code . normalizeCodeSpan $ raw)

rawAttributeSpec :: (Monad m, IsBlock il bl)
                         => SyntaxSpec m il bl
rawAttributeSpec = mempty
  { syntaxBlockSpecs = [ rawAttributeBlockSpec ]
  , syntaxInlineParsers = [ pRawSpan ]
  }

rawAttributeBlockSpec :: (Monad m, IsBlock il bl)
                              => BlockSpec m il bl
rawAttributeBlockSpec = BlockSpec
     { blockType           = "RawBlock"
     , blockStart          = try $ do
             prepos <- getPosition
             nonindentSpaces
             pos <- getPosition
             let indentspaces = sourceColumn pos - sourceColumn prepos
             (c, ticks) <-  (('`',) <$> many1 (symbol '`'))
                        <|> (('~',) <$> many1 (symbol '~'))
             let fencelength = length ticks
             guard $ fencelength >= 3
             skipWhile (hasType Spaces)
             fmt <- pRawAttribute
             skipWhile (hasType Spaces)
             lookAhead $ void lineEnd <|> eof
             addNodeToStack $
                Node (defBlockData rawAttributeBlockSpec){
                          blockData = toDyn
                               (c, fencelength, indentspaces, fmt),
                          blockStartPos = [pos] } []
             return BlockStartMatch
     , blockCanContain     = const False
     , blockContainsLines  = True
     , blockParagraph      = False
     , blockContinue       = \node -> try (do
             let ((c, fencelength, _, _)
                    :: (Char, Int, Int, Format)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, Format mempty)
             nonindentSpaces
             pos <- getPosition
             ts <- many1 (symbol c)
             guard $ length ts >= fencelength
             skipWhile (hasType Spaces)
             lookAhead $ void lineEnd <|> eof
             endOfBlock
             return $! (pos, node))
               <|> (do let ((_, _, indentspaces, _)
                              :: (Char, Int, Int, Format)) = fromDyn
                                   (blockData (rootLabel node))
                                   ('`', 3, 0, Format mempty)
                       pos <- getPosition
                       _ <- gobbleUpToSpaces indentspaces
                       return $! (pos, node))
     , blockConstructor    = \node -> do
           let ((_, _, _, fmt) :: (Char, Int, Int, Format)) =
                   fromDyn (blockData (rootLabel node))
                     ('`', 3, 0, Format mempty)
           let codetext = untokenize $ drop 1 (getBlockText node)
           -- drop 1 initial lineend token
           return $! rawBlock fmt codetext
     , blockFinalize       = defaultFinalizer
     }

-- | Allow attributes on everything.
attributesSpec
             :: (Monad m, IsInline il)
             => SyntaxSpec m il bl
attributesSpec = mempty
  { syntaxAttributeParsers = [pAttributes]
  }

pAttributes :: forall u m . Monad m => ParsecT [Tok] u m Attributes
pAttributes = mconcat <$> many1 pattr
  where
    pattr = try $ do
      symbol '{'
      optional whitespace
      let pAttribute = pIdentifier <|> pClass <|> pKeyValue
      a <- pAttribute
      as <- many $ try (whitespace *> (pIdentifier <|> pClass <|> pKeyValue))
      optional whitespace
      symbol '}'
      return $! (a:as)

pRawAttribute :: Monad m => ParsecT [Tok] u m Format
pRawAttribute = try $ do
  symbol '{'
  optional whitespace
  symbol '='
  Tok _ _ t <- satisfyWord (const True)
  optional whitespace
  symbol '}'
  return $! Format t

pIdentifier :: Monad m => ParsecT [Tok] u m Attribute
pIdentifier = try $ do
  symbol '#'
  xs <- many1 $
        satisfyWord (const True)
    <|> satisfyTok (\c -> hasType (Symbol '-') c || hasType (Symbol '_') c
                        || hasType (Symbol ':') c || hasType (Symbol '.') c)
  return $! ("id", unEntity xs)

pClass :: Monad m => ParsecT [Tok] u m Attribute
pClass = do
  symbol '.'
  xs <- many1 $
        satisfyWord (const True)
    <|> satisfyTok (\c -> hasType (Symbol '-') c || hasType (Symbol '_') c)
  return $! ("class", unEntity xs)

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
  return $! (untokenize name, unEntity val')
