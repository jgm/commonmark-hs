{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Commonmark.ReferenceMap
  ( ReferenceMap
  , insertReference
  , lookupReference
  ) where
import Data.Semigroup (Semigroup)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M

-- | Lookup table for link references.
newtype ReferenceMap = ReferenceMap (M.Map Text (Text, Text))
  deriving (Show, Semigroup, Monoid)

-- | Insert a link reference into a reference map.
insertReference :: Text -- ^ Reference label
                -> (Text, Text) -- ^ (URI, title)
                -> ReferenceMap
                -> ReferenceMap
insertReference label (target, title) (ReferenceMap m) =
  ReferenceMap (M.insertWith (\_new old -> old)
    (T.toCaseFold $ normalizeSpaces label) (target, title) m)

-- | Lookup a link reference in a reference map.
lookupReference :: Text -- ^ Reference label
                -> ReferenceMap
                -> Maybe (Text, Text) -- ^ (URI, title)
lookupReference label (ReferenceMap m) =
  M.lookup (T.toCaseFold $ normalizeSpaces label) m

normalizeSpaces :: Text -> Text
normalizeSpaces = T.unwords . T.words
