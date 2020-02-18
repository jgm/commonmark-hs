{- |

The basic task of this library is to parse text as commonmark. The input
is a list of tokens (@['Tok']@), which can be be produced from a 'Text'
using 'tokenize'.  Parsing is done by 'parseCommonmark' or
'parseCommonmarkWith'.  Here is a simple usage example:

> {-# LANGUAGE ScopedTypeVariables #-}
> import Commonmark
> import Data.Text.IO as TIO
> import Data.Text.Lazy.IO as TLIO
>
> main = do
>   inp <- TIO.getContents
>   case parseCommonmark (tokenize "stdin" inp) of
>        Left e                  -> error (show e)
>        Right (html :: Html ()) -> TLIO.putStr (renderHtml html)

The parser is highly polymorphic: in this example, we use
the type annotation @'Html' ()@ to indicate that we want it
to produce basic HTML without source location attributes
(for that we would use @'Html' 'SourceRange'@).

Extensibility is emphasized throughout. To change the output
for a given format, or support an alternate output format,
one has only to define instances of 'IsBlock' and 'IsInline'
for a new type.  (For an example of this kind of extension,
see the @commonmark-pandoc@ package, which defines these
instances for pandoc's native types.)

Supporting a new syntactic element generally requires (a) adding
a 'SyntaxSpec' for it and (b) defining new type classes.
See the examples in the @commonmark-extensions@ package.
Note that 'SyntaxSpec' is a Monoid, so one can extend
'defaultSyntaxSpec' by specifying @myNewSyntaxSpec <> defaultSyntaxSpec@.

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

