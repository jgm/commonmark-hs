{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commonmark.ReferenceMap
  ( ReferenceMap
  , LinkInfo(..)
  , emptyReferenceMap
  , insertReference
  , lookupReference
  ) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Dynamic
import Data.Typeable (Typeable)
import Debug.Trace

-- | Lookup table for link references.
newtype ReferenceMap = ReferenceMap (M.Map Text [Dynamic])
  deriving (Show)

data LinkInfo = LinkInfo{ linkDestination :: Text
                        , linkTitle       :: Text }
     deriving (Show, Typeable)

emptyReferenceMap :: ReferenceMap
emptyReferenceMap = ReferenceMap M.empty

-- | Insert a link reference into a reference map.
insertReference :: Typeable a
                => Text -- ^ Reference label
                -> a
                -> ReferenceMap
                -> ReferenceMap
insertReference label x (ReferenceMap m) =
  ReferenceMap (M.insertWith (\new old -> old ++ new)
    (T.toCaseFold $ normalizeSpaces label) [toDyn x] m)

-- | Lookup a reference in a reference map.  If there are several
-- values at this key, we return the first one in the list that
-- can be converted to an 'a'.
lookupReference :: Typeable a
                => Text -- ^ Reference label
                -> ReferenceMap
                -> Maybe a
lookupReference label (ReferenceMap m) =
  getFirst $ M.lookup (T.toCaseFold $ normalizeSpaces label) m
  where getFirst Nothing       = Nothing
        getFirst (Just [])     = Nothing
        getFirst (Just (x:xs)) = case fromDynamic (traceShowId x) of
                                      Just v  -> Just v
                                      Nothing -> trace "moving on" $ getFirst (Just xs)

normalizeSpaces :: Text -> Text
normalizeSpaces = T.unwords . T.words
