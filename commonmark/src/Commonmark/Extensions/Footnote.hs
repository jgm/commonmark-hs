{-# LANGUAGE CPP #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Footnote
  ( footnoteSpec
  , HasFootnote(..)
  )
where
import Commonmark.Tokens
import Commonmark.Types
import Commonmark.Html
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Util
import Commonmark.ReferenceMap
import Control.Monad.Trans.Class (lift)
import Control.Monad (mzero)
import Data.List
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (Semigroup)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif
import Data.Dynamic
import Data.Tree
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

data FootnoteDef bl m =
  FootnoteDef Int Text (ReferenceMap -> m (Either ParseError bl))
  deriving Typeable

instance Eq (FootnoteDef bl m) where
  FootnoteDef num1 lab1 _ == FootnoteDef num2 lab2 _
    = num1 == num2 && lab1 == lab2

instance Ord (FootnoteDef bl m) where
  (FootnoteDef num1 lab1 _) `compare` (FootnoteDef num2 lab2 _) =
    (num1, lab1) `compare` (num2, lab2)

footnoteSpec :: (Monad m, Typeable m, IsBlock il bl, IsInline il,
                 Typeable il, Typeable bl, HasFootnote il bl)
             => SyntaxSpec m il bl
footnoteSpec = SyntaxSpec
  { syntaxBlockSpecs = [footnoteBlockSpec]
  , syntaxBracketedSpecs = []
  , syntaxFormattingSpecs = []
  , syntaxInlineParsers = [pFootnoteRef]
  , syntaxFinalParsers = [addFootnoteList]
  }

footnoteBlockSpec :: (Monad m, Typeable m, Typeable il, Typeable bl,
                      IsBlock il bl, IsInline il, HasFootnote il bl)
                  => BlockSpec m il bl
footnoteBlockSpec = BlockSpec
     { blockType           = "Footnote"
     , blockStart          = try $ do
             nonindentSpaces
             pos <- getPosition
             lab' <- pFootnoteLabel
             _ <- symbol ':'
             counters' <- counters <$> getState
             let num = fromMaybe (1 :: Int) $
                       M.lookup "footnote" counters' >>= fromDynamic
             updateState $ \s -> s{ counters =
                                     M.insert "footnote" (toDyn (num + 1))
                                      (counters s) }
             addNodeToStack $
                Node (defBlockData footnoteBlockSpec){
                            blockData = toDyn (num, lab')
                          , blockStartPos = [pos] } []
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
     , blockFinalize       = \(Node root children) parent -> do
         let (num, lab') = fromDyn (blockData root) (1, mempty)
         st <- getState
         let mkNoteContents refmap =
               runParserT
                 (blockConstructor (blockSpec root) (Node root children))
                 st{ referenceMap = refmap }
                 "source" []
         updateState $ \s -> s{
             referenceMap = insertReference lab'
                              (FootnoteDef num lab' mkNoteContents)
                              (referenceMap s)
             }
         return parent
     }

pFootnoteLabel :: Monad m => ParsecT [Tok] u m Text
pFootnoteLabel = try $ do
  lab <- pLinkLabel
  case T.uncons lab of
        Just ('^', t') -> return t'
        _ -> mzero

pFootnoteRef :: (Monad m, Typeable m, Typeable a,
                 Typeable b, IsInline a, IsBlock a b, HasFootnote a b)
             => InlineParser m a
pFootnoteRef = try $ do
  lab <- pFootnoteLabel
  rm <- ipReferenceMap <$> getState
  case lookupReference lab rm of
        Just (FootnoteDef num _ mkContents) -> do
          res <- lift $ mkContents rm
          case res of
               Left err -> mkPT (\_ -> return (Empty (return (Error err))))
               Right contents -> return $
                 footnoteRef (T.pack (show num)) lab contents
        Nothing -> mzero

addFootnoteList :: (Monad m, Typeable m, Typeable bl, HasFootnote il bl,
                    IsBlock il bl) => BlockParser m il bl bl
addFootnoteList = do
  rm <- referenceMap <$> getState
  let keys = M.keys . unReferenceMap $ rm
  let getNote key = lookupReference key rm
  let notes = sort $ mapMaybe getNote keys
  let renderNote (FootnoteDef num lab mkContents) = do
        res <- lift $ mkContents rm
        case res of
             Left err -> mkPT (\_ -> return (Empty (return (Error err))))
             Right contents -> return $ footnote num lab contents
  if null notes
     then return mempty
     else footnoteList <$> mapM renderNote notes

class IsBlock il bl => HasFootnote il bl | il -> bl where
  footnote :: Int -> Text -> bl -> bl
  footnoteList :: [bl] -> bl
  footnoteRef :: Text -> Text -> bl -> il

instance HasFootnote Html5 Html5 where
  footnote num lab' x = "<div class=\"footnote\" id=\""
    <> Html5 (escapeHtml ("fn-" <> lab'))
    <> "\">\n"
    <> "<div class=\"footnote-number\">\n"
    <> "<a href=\""
    <> Html5 (escapeHtml ("#fnref-" <> lab'))
    <> "\">"
    <> Html5 (escapeHtml (T.pack (show num)))
    <> "</a>\n"
    <> "</div>\n"
    <> "<div class=\"footnote-contents\">\n"
    <> x
    <> "</div>\n</div>\n"
  footnoteList items = "<section class=\"footnotes\">\n" <>
    mconcat items <> "</section>\n"
  footnoteRef x lab _ = "<sup class=\"footnote-ref\">"
    <> "<a href=\""
    <> Html5 (escapeHtml ("#fn-" <> lab))
    <> "\" id=\""
    <> Html5 (escapeHtml ("fnref-" <> lab))
    <> "\">"
    <> str x
    <> "</a></sup>"

instance (HasFootnote il bl, Semigroup bl, Semigroup il)
        => HasFootnote (WithSourceMap il) (WithSourceMap bl) where
  footnote num lab' x = (footnote num lab' <$> x) <* addName "footnote"
  footnoteList items = footnoteList items
  footnoteRef x y z = (footnoteRef x y <$> z) <* addName "footnoteRef"
