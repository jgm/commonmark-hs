module Commonmark.Parser
    ( parseCommonmark
    , parseCommonmarkWith
    , module Commonmark.Types
    , module Commonmark.Syntax
    -- * Exported from "Text.Parsec.Error"
    , ParseError
    ) where

import           Commonmark.Blocks
import           Commonmark.Inlines
import           Commonmark.Types
import           Commonmark.Syntax (SyntaxSpec(..), defaultSyntaxSpec)
import           Text.Parsec.Error (ParseError)
import           Data.Functor.Identity   (runIdentity)
import           Data.Text (Text)

-- | Parse a commonmark document using the core syntax
-- elements.  If you want to add syntax extensions or run the parser in
-- a monad, use 'parseCommonmarkWith'.  Simple usage example:
--
-- @
-- {-# LANGUAGE ScopedTypeVariables #-}
-- import Commonmark
-- import Data.Text.IO as TIO
-- import Data.Text.Lazy.IO as TLIO
--
-- main = do
--   inp <- TIO.getContents
--   case parseCommonmark "stdin" inp of
--        Left e     -> error (show e)
--        Right (html :: Html ()) -> TLIO.putStr (renderHtml html)
-- @
parseCommonmark :: IsBlock il bl
                => String   -- ^ Name of input file or source
                -> Text      -- ^ Commonmark input
                -> Either ParseError bl  -- ^ Result or error
parseCommonmark sourcename =
  runIdentity . parseCommonmarkWith defaultSyntaxSpec sourcename

-- | Parse a commonmark document using specified syntax elements.
parseCommonmarkWith :: (Monad m, IsBlock il bl, IsInline il)
                    => SyntaxSpec m il bl -- ^ Defines syntax
                    -> String  -- ^ Name of input file or source
                    -> Text    -- ^ Tokenized commonmark input
                    -> m (Either ParseError bl)  -- ^ Result or error
parseCommonmarkWith syntax =
    mkBlockParser (syntaxBlockSpecs syntax)
      (syntaxFinalParsers syntax)
      (mkInlineParser (syntaxBracketedSpecs syntax)
                      (syntaxFormattingSpecs syntax)
                      (syntaxInlineParsers syntax)
                      (syntaxAttributeParsers syntax))
      (syntaxAttributeParsers syntax)
