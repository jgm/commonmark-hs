{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Commonmark.ReferenceMap
  ( ReferenceMap(..)
  , LinkInfo(..)
  , emptyReferenceMap
  , insertReference
  , lookupReference
  ) where
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Data.Dynamic
import Commonmark.Types
#if !MIN_VERSION_base(4,13,0)
import Data.Typeable (Typeable)
#endif

-- | Lookup table for link references.
newtype ReferenceMap = ReferenceMap { unReferenceMap :: M.Map Text [Dynamic] }
  deriving (Show)

data LinkInfo = LinkInfo{ linkDestination :: !Text
                        , linkTitle       :: !Text
                        , linkAttributes  :: !Attributes
                        , linkPos         :: !(Maybe SourcePos)
                            -- ^ Position of the reference link definition
                            -- for references links.
                        }
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
  ReferenceMap (M.insertWith (flip (++))
    (T.toCaseFold $! normalizeSpaces label) [toDyn x] m)

-- | Lookup a reference in a reference map.  If there are several
-- values at this key, we return the first one in the list that
-- can be converted to an 'a'.
lookupReference :: Typeable a
                => Text -- ^ Reference label
                -> ReferenceMap
                -> Maybe a
lookupReference label (ReferenceMap m) =
  getFirst $! M.lookup (T.toCaseFold $! normalizeSpaces label) m
  where getFirst Nothing       = Nothing
        getFirst (Just [])     = Nothing
        getFirst (Just (x:xs)) = case fromDynamic x of
                                      Just !v  -> Just v
                                      Nothing  -> getFirst (Just xs)

normalizeSpaces :: Text -> Text
normalizeSpaces = T.unwords . T.words
