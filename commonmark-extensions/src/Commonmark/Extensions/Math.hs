{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Commonmark.Extensions.Math
  ( HasMath(..)
  , mathSpec )
where
import Control.Monad (mzero)
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T

mathSpec :: (Monad m, IsBlock il bl, IsInline il, HasMath il)
         => SyntaxSpec m il bl
mathSpec = mempty
  { syntaxInlineParsers = [withAttributes parseMath]
  }

class HasMath a where
  inlineMath :: Text -> a
  displayMath :: Text -> a

instance HasMath (Html a) where
  inlineMath t = addAttribute ("class", "math inline") $
    htmlInline "span" $ Just $ htmlRaw "\\(" <> htmlText t <> htmlRaw "\\)"
  displayMath t = addAttribute ("class", "math display") $
    htmlInline "span" $ Just $ htmlRaw "\\[" <> htmlText t <> htmlRaw "\\]"

instance (HasMath i, Monoid i) => HasMath (WithSourceMap i) where
  inlineMath t = (inlineMath t) <$ addName "inlineMath"
  displayMath t = (displayMath t) <$ addName "displayMath"

parseMath :: (Monad m, HasMath a) => InlineParser m a
parseMath = try $ do
  symbol '$'
  display <- (True <$ symbol '$') <|> (False <$ notFollowedBy whitespace)
  contents <- try $ untokenize <$> pDollarsMath 0
  if display
     then displayMath contents <$ symbol '$'
     else if T.all (==' ') (T.takeEnd 1 contents)
             -- don't allow math to end with SPACE + $
             then mzero
             else return $ inlineMath contents

-- Int is number of embedded groupings
pDollarsMath :: Monad m => Int -> InlineParser m [Tok]
pDollarsMath n = do
  tk@(Tok toktype _ _) <- anyTok
  case toktype of
       Symbol '$'
              | n == 0 -> return []
       Symbol '\\' -> do
              tk' <- anyTok
              (tk :) . (tk' :) <$> pDollarsMath n
       Symbol '{' -> (tk :) <$> pDollarsMath (n+1)
       Symbol '}' | n > 0 -> (tk :) <$> pDollarsMath (n-1)
                  | otherwise -> mzero
       _ -> (tk :) <$> pDollarsMath n
