module Commonmark.Parser
    ( parseCommonmark
    , parseCommonmarkWith
    , module Commonmark.Tokens
    , module Commonmark.Types
    , module Commonmark.Syntax
    -- * Exported from "Text.Parsec.Error"
    , ParseError
    ) where

import           Commonmark.Blocks
import           Commonmark.Inlines
import           Commonmark.Tokens
import           Commonmark.Types
import           Commonmark.Syntax (SyntaxSpec(..), defaultSyntaxSpec)
import           Text.Parsec.Error (ParseError)
import           Data.Functor.Identity   (runIdentity)

-- | Parse a tokenized commonmark document using the core syntax
-- elements.  Use 'tokenize' to convert 'Text' into ['Tok'].
-- If you want to add syntax extensions or run the parser in a monad,
-- use 'parseCommonmarkWith'.  Simple usage example:
--
-- @
-- {-# LANGUAGE ScopedTypeVariables #-}
-- import Commonmark
-- import Data.Text.IO as TIO
-- import Data.Text.Lazy.IO as TLIO
--
-- main = do
--   inp <- TIO.getContents
--   case parseCommonmark (tokenize "stdin" inp) of
--        Left e     -> error (show e)
--        Right (html :: Html ()) -> TLIO.putStr (renderHtml html)
-- @
parseCommonmark :: IsBlock il bl
                => [Tok] -- ^ Tokenized commonmark input
                -> Either ParseError bl  -- ^ Result or error
parseCommonmark = runIdentity . parseCommonmarkWith defaultSyntaxSpec

-- | Parse a tokenized commonmark document using specified
-- syntax elements.
parseCommonmarkWith :: (Monad m, IsBlock il bl, IsInline il)
                    => SyntaxSpec m il bl -- ^ Defines syntax
                    -> [Tok] -- ^ Tokenized commonmark input
                    -> m (Either ParseError bl)  -- ^ Result or error
parseCommonmarkWith syntax =
    mkBlockParser (syntaxBlockSpecs syntax)
      (syntaxFinalParsers syntax)
      (mkInlineParser (syntaxBracketedSpecs syntax)
                      (syntaxFormattingSpecs syntax)
                      (syntaxInlineParsers syntax))
