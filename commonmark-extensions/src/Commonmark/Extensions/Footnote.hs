{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Commonmark.TokParsers
import Commonmark.ReferenceMap
import Control.Monad.Trans.Class (lift)
import Control.Monad (mzero)
import Data.List
import Data.Maybe (fromMaybe, mapMaybe)
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
footnoteSpec = mempty
  { syntaxBlockSpecs = [footnoteBlockSpec]
  , syntaxInlineParsers = [withAttributes pFootnoteRef]
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
             return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> try $ do
             () <$ (gobbleSpaces 4)
               <|> (skipWhile (hasType Spaces) >> () <$ lookAhead lineEnd)
             pos <- getPosition
             return $! (pos, n)
     , blockConstructor    = \node ->
          mconcat <$> mapM (\n ->
              blockConstructor (blockSpec (rootLabel n)) n)
           (subForest (reverseSubforests node))
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
         return $! parent
     }

pFootnoteLabel :: Monad m => ParsecT [Tok] u m Text
pFootnoteLabel = try $ do
  lab <- pLinkLabel
  case T.uncons lab of
        Just ('^', t') -> return $! t'
        _ -> mzero

pFootnoteRef :: (Monad m, Typeable m, Typeable a,
                 Typeable b, IsInline a, IsBlock a b, HasFootnote a b)
             => InlineParser m a
pFootnoteRef = try $ do
  lab <- pFootnoteLabel
  rm <- getReferenceMap
  case lookupReference lab rm of
        Just (FootnoteDef num _ mkContents) -> do
          res <- lift . lift $ mkContents rm
          case res of
               Left err -> mkPT (\_ -> return (Empty (return (Error err))))
               Right contents -> return $!
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
             Right contents -> return $! footnote num lab contents
  if null notes
     then return mempty
     else footnoteList <$> mapM renderNote notes

class IsBlock il bl => HasFootnote il bl | il -> bl where
  footnote :: Int -> Text -> bl -> bl
  footnoteList :: [bl] -> bl
  footnoteRef :: Text -> Text -> bl -> il

instance Rangeable (Html a) => HasFootnote (Html a) (Html a) where
  footnote num lab' x =
    addAttribute ("class", "footnote") $
    addAttribute ("id", "fn-" <> lab') $
    htmlBlock "div" $ Just $ htmlRaw "\n" <>
      (addAttribute ("class", "footnote-number") $
       htmlBlock "div" $ Just $ htmlRaw "\n" <>
        (addAttribute ("href", "#fnref-" <> lab') $
         htmlInline "a" (Just $ htmlText $ T.pack $ show num)) <>
         htmlRaw "\n") <>
      (addAttribute ("class", "footnote-contents") $
        htmlBlock "div" $ Just $ htmlRaw "\n" <> x)
  footnoteList items =
    addAttribute ("class", "footnotes") $
      htmlBlock "section" $ Just $ htmlRaw "\n" <> mconcat items
  footnoteRef x lab _ =
   addAttribute ("class", "footnote-ref") $
     htmlInline "sup" $ Just $
       addAttribute ("href", "#fn-" <> lab) $
       addAttribute ("id", "fnref-" <> lab) $
       htmlInline "a" $ Just (htmlText x)

instance (HasFootnote il bl, Semigroup bl, Semigroup il)
        => HasFootnote (WithSourceMap il) (WithSourceMap bl) where
  footnote num lab' x = (footnote num lab' <$> x) <* addName "footnote"
  footnoteList items = footnoteList <$> sequence items
  footnoteRef x y z = (footnoteRef x y <$> z) <* addName "footnoteRef"
