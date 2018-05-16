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

-- | Lookup table for link references.
newtype ReferenceMap = ReferenceMap (M.Map Text Dynamic)
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
  ReferenceMap (M.insertWith (\_new old -> old)
    (T.toCaseFold $ normalizeSpaces label) (toDyn x) m)

-- | Lookup a link reference in a reference map.
lookupReference :: Typeable a
                => Text -- ^ Reference label
                -> ReferenceMap
                -> Maybe a
lookupReference label (ReferenceMap m) =
  M.lookup (T.toCaseFold $ normalizeSpaces label) m >>= fromDynamic

normalizeSpaces :: Text -> Text
normalizeSpaces = T.unwords . T.words
