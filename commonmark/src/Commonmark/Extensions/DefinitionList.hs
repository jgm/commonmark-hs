{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.DefinitionList
  ( definitionListSpec
  , HasDefinitionList(..)
  )
where
import Commonmark.Tokens
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.SourceMap
import Commonmark.Util
import Data.Semigroup (Semigroup)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Data.Dynamic
import Data.Tree
import Text.Parsec
import Data.Text.Lazy.Builder (Builder)

definitionListSpec :: (Monad m, Typeable m, IsBlock il bl, IsInline il,
                       Typeable il, Typeable bl, HasDefinitionList il bl)
                   => SyntaxSpec m il bl
definitionListSpec = SyntaxSpec
  { syntaxBlockSpecs = [definitionListBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = []
  , syntaxFinalParsers = []
  }

definitionListBlockSpec :: (Monad m, Typeable m, Typeable il, Typeable bl,
                           IsBlock il bl, IsInline il,
                           HasDefinitionList il bl)
                         => BlockSpec m il bl
definitionListBlockSpec = BlockSpec
     { blockType           = "DefinitionList"
     , blockStart          = try $ do
             nonindentSpaces
             pos <- getPosition
             addNodeToStack $
                Node (defBlockData definitionListBlockSpec){
                           blockStartPos = [pos] } []
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> try $ do
             () <$ (gobbleSpaces 4)
               <|> (skipWhile (hasType Spaces) >> () <$ lookAhead lineEnd)
             pos <- getPosition
             return (pos, n)
     , blockConstructor    = \node ->
          (addRange node . mconcat) <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
            (reverse (subForest node))
     , blockFinalize       = defaultFinalizer
     }

class IsBlock il bl => HasDefinitionList il bl | il -> bl where
  definitionList :: [(il,[bl])] -> bl

instance HasDefinitionList Builder Builder where
  definitionList items =
    "<dl>" <> mconcat (map definitionListItem items) <> "</dl>"

definitionListItem :: (Builder, [Builder]) -> Builder
definitionListItem (term, defns) =
  "<dt>" <> term <> "</dt>\n" <>
  mconcat (map (\defn -> "<dd>\n" <> defn <> "</dd>\n") defns)

instance (HasDefinitionList il bl, Semigroup bl, Semigroup il)
        => HasDefinitionList (WithSourceMap il) (WithSourceMap bl) where
  definitionList items = definitionList items
