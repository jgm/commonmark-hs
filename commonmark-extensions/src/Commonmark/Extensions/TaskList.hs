{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.TaskList
  ( taskListSpec
  , HasTaskList (..)
  )
where
import Commonmark.Tokens
import Commonmark.Types
import Commonmark.Syntax
import Commonmark.Blocks
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Control.Monad (mzero)
import Control.Monad (when, guard)
import Data.List (sort)
import Data.Dynamic
import Data.Tree
import Text.Parsec


taskListSpec :: (Monad m, IsBlock il bl, IsInline il, HasTaskList il bl)
                   => SyntaxSpec m il bl
taskListSpec = mempty
  { syntaxBlockSpecs = [taskListItemBlockSpec]
  }

data ListData = ListData
     { listType    :: !ListType
     , listSpacing :: !ListSpacing
     } deriving (Show, Eq)

data ListItemData = ListItemData
     { listItemType         :: !ListType
     , listItemChecked      :: !Bool
     , listItemIndent       :: !Int
     , listItemBlanksInside :: !Bool
     , listItemBlanksAtEnd  :: !Bool
     } deriving (Show, Eq)

taskListBlockSpec :: (Monad m, IsBlock il bl,
                      HasTaskList il bl) => BlockSpec m il bl
taskListBlockSpec = BlockSpec
     { blockType           = "TaskList"
     , blockStart          = mzero
     , blockCanContain     = \sp -> blockType sp == "TaskListItem"
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \n -> (,n) <$> getPosition
     , blockConstructor    = \node -> do
          let ListData lt ls = fromDyn (blockData (rootLabel node))
                                 (ListData (BulletList '*') TightList)
          let getCheckedStatus n =
               listItemChecked $
                      fromDyn (blockData (rootLabel n))
                         (ListItemData (BulletList '*') False 0 False False)
          let checkedStatus = map getCheckedStatus $ subForest node
          taskList lt ls . zip checkedStatus <$> renderChildren node
     , blockFinalize       = \(Node cdata children) parent -> do
          let ListData lt _ = fromDyn (blockData cdata)
                                 (ListData (BulletList '*') TightList)
          let getListItemData (Node d _) =
                fromDyn (blockData d)
                  (ListItemData (BulletList '*') False 0 False False)
          let childrenData = map getListItemData children
          let ls = case childrenData of
                          c:cs | any listItemBlanksInside (c:cs) ||
                                 (not (null cs) &&
                                  any listItemBlanksAtEnd cs)
                               -> LooseList
                          _    -> TightList
          blockBlanks' <- case childrenData of
                             c:_ | listItemBlanksAtEnd c -> do
                                 curline <- sourceLine <$> getPosition
                                 return $! curline - 1 : blockBlanks cdata
                             _ -> return $! blockBlanks cdata
          let ldata' = toDyn (ListData lt ls)
          -- need to transform paragraphs on tight lists
          let totight (Node nd cs)
                | blockType (blockSpec nd) == "Paragraph"
                            = Node nd{ blockSpec = plainSpec } cs
                | otherwise = Node nd cs
          let childrenToTight (Node nd cs) = Node nd (map totight cs)
          let children' =
                 if ls == TightList
                    then map childrenToTight children
                    else children
          defaultFinalizer (Node cdata{ blockData = ldata'
                                      , blockBlanks = blockBlanks' } children')
                           parent
     }

taskListItemBlockSpec :: (Monad m, IsBlock il bl, HasTaskList il bl)
                      => BlockSpec m il bl
taskListItemBlockSpec = BlockSpec
     { blockType           = "TaskListItem"
     , blockStart          = do
             (pos, lidata) <- itemStart
             let linode = Node (defBlockData taskListItemBlockSpec){
                             blockData = toDyn lidata,
                             blockStartPos = [pos] } []
             let listdata = ListData{
                    listType = listItemType lidata
                  , listSpacing = TightList }
                  -- spacing gets set in finalize
             let listnode = Node (defBlockData taskListBlockSpec){
                              blockData = toDyn listdata,
                              blockStartPos = [pos] } []
             -- list can only interrupt paragraph if bullet
             -- list or ordered list w/ startnum == 1,
             -- and not followed by blank
             (cur:_) <- nodeStack <$> getState
             when (blockParagraph (bspec cur)) $ do
               guard $ case listType listdata of
                            BulletList _            -> True
                            OrderedList 1 Decimal _ -> True
                            _                       -> False
               notFollowedBy blankLine
             let curdata = fromDyn (blockData (rootLabel cur))
                                (ListData (BulletList '*') TightList)
             let matchesList (BulletList c) (BulletList d)       = c == d
                 matchesList (OrderedList _ e1 d1)
                             (OrderedList _ e2 d2) = e1 == e2 && d1 == d2
                 matchesList _ _                                 = False
             case blockType (bspec cur) of
                  "TaskList" | listType curdata `matchesList`
                               listItemType lidata
                    -> addNodeToStack linode
                  _ -> addNodeToStack listnode >> addNodeToStack linode
             return BlockStartMatch
     , blockCanContain     = const True
     , blockContainsLines  = False
     , blockParagraph      = False
     , blockContinue       = \node@(Node ndata children) -> do
             let lidata = fromDyn (blockData ndata)
                             (ListItemData (BulletList '*') False 0
                              False False)
             -- a marker followed by two blanks is just an empty item:
             guard $ null (blockBlanks ndata) ||
                     not (null children)
             pos <- getPosition
             gobbleSpaces (listItemIndent lidata) <|> 0 <$ lookAhead blankLine
             return $! (pos, node)
     , blockConstructor    = fmap mconcat . renderChildren
     , blockFinalize       = \(Node cdata children) parent -> do
          let lidata = fromDyn (blockData cdata)
                                 (ListItemData (BulletList '*') False
                                   0 False False)
          let blanks = removeConsecutive $ sort $
                         concat $ blockBlanks cdata :
                                  map (blockBlanks . rootLabel)
                                  (filter ((== "List") . blockType .
                                   blockSpec . rootLabel) children)
          curline <- sourceLine <$> getPosition
          let blanksAtEnd = case blanks of
                                   (l:_) -> l >= curline - 1
                                   _     -> False
          let blanksInside = case length blanks of
                                n | n > 1     -> True
                                  | n == 1    -> not blanksAtEnd
                                  | otherwise -> False
          let lidata' = toDyn $ lidata{ listItemBlanksInside = blanksInside
                                      , listItemBlanksAtEnd  = blanksAtEnd }
          defaultFinalizer (Node cdata{ blockData = lidata' } children)
                           parent
     }

removeConsecutive :: [Int] -> [Int]
removeConsecutive (x:y:zs)
  | x == y + 1 = removeConsecutive (y:zs)
removeConsecutive xs = xs

itemStart :: Monad m
          => BlockParser m il bl (SourcePos, ListItemData)
itemStart = do
  beforecol <- sourceColumn <$> getPosition
  gobbleUpToSpaces 3
  pos <- getPosition
  ty <- bulletListMarker
  aftercol <- sourceColumn <$> getPosition
  checked <- parseCheckbox
  lookAhead whitespace
  numspaces <- try (gobbleUpToSpaces 4 <* notFollowedBy whitespace)
           <|> gobbleSpaces 1
           <|> 1 <$ lookAhead lineEnd
  return $! (pos, ListItemData{
            listItemType = ty
          , listItemChecked = checked
          , listItemIndent = (aftercol - beforecol) + numspaces
          , listItemBlanksInside = False
          , listItemBlanksAtEnd = False
          })

parseCheckbox :: Monad m => BlockParser m il bl Bool
parseCheckbox = do
  gobbleUpToSpaces 3
  symbol '['
  checked <- (False <$ satisfyTok (hasType Spaces))
         <|> (True  <$ satisfyTok (textIs (\t -> t == "x" || t == "X")))
  symbol ']'
  return checked

class IsBlock il bl => HasTaskList il bl where
  taskList :: ListType -> ListSpacing -> [(Bool, bl)] -> bl

instance Rangeable (Html a) => HasTaskList (Html a) (Html a) where
  taskList lt spacing items =
    addAttribute ("class","task-list")
    $ list lt spacing
    $ map addCheckbox items

addCheckbox :: (Bool, Html a) -> Html a
addCheckbox (checked, x) =
  (addAttribute ("type", "checkbox") $
   addAttribute ("disabled", "") $
   (if checked then addAttribute ("checked","") else id) $
   htmlInline "input" Nothing) <> x

instance (HasTaskList il bl, Semigroup bl, Semigroup il)
        => HasTaskList (WithSourceMap il) (WithSourceMap bl) where
   taskList lt spacing items =
     (do let (checks, xs) = unzip items
         taskList lt spacing . zip checks <$> sequence xs
      ) <* addName "taskList"
