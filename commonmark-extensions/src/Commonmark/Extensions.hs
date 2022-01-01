{- |

Syntax extensions for the commonmark library.
Usage example:

> {-# LANGUAGE ScopedTypeVariables #-}
> import Commonmark
> import Commonmark.Extensions
> import Data.Text.IO as TIO
> import Data.Text.Lazy.IO as TLIO
>
> main :: IO ()
> main = do
>   let customSyntax =
>          (mathSpec <> smartPunctuationSpec <> defaultSyntaxSpec)
>   inp <- TIO.getContents
>   res <- commonmarkWith customSyntax "stdin" inp
>   case res of
>     Left e                  -> error (show e)
>     Right (html :: Html ()) -> TLIO.putStr $ renderHtml html

-}

module Commonmark.Extensions
    ( module Commonmark.Extensions.Smart
    , module Commonmark.Extensions.HardLineBreaks
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
    , module Commonmark.Extensions.TaskList
    , module Commonmark.Extensions.ImplicitHeadingReferences
    , module Commonmark.Extensions.Wikilinks
    , module Commonmark.Extensions.RebaseRelativePaths
    , gfmExtensions
    ) where

import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.HardLineBreaks
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
import           Commonmark.Extensions.TaskList
import           Commonmark.Extensions.ImplicitHeadingReferences
import           Commonmark.Extensions.Wikilinks
import           Commonmark.Extensions.RebaseRelativePaths
import           Commonmark
import           Data.Typeable

-- | Standard extensions for GitHub-flavored Markdown.
gfmExtensions :: (Monad m, Typeable m, Typeable il, Typeable bl,
                  IsBlock il bl, IsInline il, HasFootnote il bl,
                  HasEmoji il, HasStrikethrough il, HasPipeTable il bl,
                  HasTaskList il bl, ToPlainText il)
              => SyntaxSpec m il bl
gfmExtensions =
  emojiSpec <> strikethroughSpec <> pipeTableSpec <> autolinkSpec <>
    autoIdentifiersSpec <> taskListSpec <> footnoteSpec

