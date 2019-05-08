module Commonmark.Attributes
  ( Attribute
  , Attributes
  , HasAttributes(..)
  )
where
import Data.Text (Text)

type Attribute = (Text, Text)

type Attributes = [Attribute]

class HasAttributes a where
  addAttributes :: Attributes -> a -> a
