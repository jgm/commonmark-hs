{- |

Syntax extensions for the commonmark library.
Usage example:

> {-# LANGUAGE ScopedTypeVariables #-}
> import Commonmark
> import Commonmark.Extensions
> import Data.Text.IO as TIO
> import Data.Text.Lazy.IO as TLIO
> import Data.Functor.Identity (runIdentity)
>
> main = do
>   inp <- TIO.getContents
>   let customParser = runIdentity . parseCommonmarkWith
>         (mathSpec <> smartPunctuationSpec <> defaultSyntaxSpec)
>   case customParser (tokenize "stdin" inp) of
>        Left e                  -> error (show e)
>        Right (html :: Html ()) -> TLIO.putStr (renderHtml html)

-}

module Commonmark.Extensions
    ( module Commonmark.Extensions.Smart
    , module Commonmark.Extensions.Strikethrough
    , module Commonmark.Extensions.Superscript
    , module Commonmark.Extensions.Subscript
    , module Commonmark.Extensions.PipeTable
    , module Commonmark.Extensions.Math
    , module Commonmark.Extensions.Emoji
    , module Commonmark.Extensions.Autolink
    , module Commonmark.Extensions.Footnote
    , module Commonmark.Extensions.DefinitionList
    , module Commonmark.Extensions.Attributes
    , module Commonmark.Extensions.AutoIdentifiers
    , module Commonmark.Extensions.FancyList
    , module Commonmark.Extensions.ImplicitHeadingReferences
    ) where

import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.Strikethrough
import           Commonmark.Extensions.Superscript
import           Commonmark.Extensions.Subscript
import           Commonmark.Extensions.PipeTable
import           Commonmark.Extensions.Math
import           Commonmark.Extensions.Emoji
import           Commonmark.Extensions.Autolink
import           Commonmark.Extensions.Footnote
import           Commonmark.Extensions.DefinitionList
import           Commonmark.Extensions.Attributes
import           Commonmark.Extensions.AutoIdentifiers
import           Commonmark.Extensions.FancyList
import           Commonmark.Extensions.ImplicitHeadingReferences

