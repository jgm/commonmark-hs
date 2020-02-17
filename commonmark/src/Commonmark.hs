{- |
   Simple usage example:

> {-# LANGUAGE ScopedTypeVariables #-}
> import Commonmark
> import Data.Text.IO as TIO
> import Data.Text.Lazy.IO as TLIO
>
> main = do
>   inp <- TIO.getContents
>   case parseCommonmark (tokenize "stdin" inp) of
>        Left e     -> error (show e)
>        Right (html :: Html ()) -> TLIO.putStr (renderHtml html)

-}

module Commonmark
    ( module Commonmark.Tokens
    , module Commonmark.Types
    , module Commonmark.Syntax
    , module Commonmark.Parser
    , module Commonmark.SourceMap
    , module Commonmark.Html
    ) where

import           Commonmark.Tokens
import           Commonmark.Types
import           Commonmark.Parser
import           Commonmark.Syntax
import           Commonmark.SourceMap
import           Commonmark.Html

