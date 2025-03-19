{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Commonmark.Extensions.Emoji
  ( HasEmoji(..)
  , emojiSpec )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.TokParsers
import Commonmark.Html
import Commonmark.Nodes
import Text.Emoji (emojiFromAlias)
import Text.Parsec
import Data.Text (Text)
import Data.Typeable (Typeable)

emojiSpec :: (Monad m, IsBlock il bl, IsInline il, HasEmoji il)
          => SyntaxSpec m il bl
emojiSpec = mempty
  { syntaxInlineParsers = [withAttributes parseEmoji]
  }

class HasEmoji a where
  emoji :: Text   -- the ascii keyword
        -> Text   -- the emoji characters
        -> a

instance HasEmoji (Html a) where
  emoji kw t = addAttribute ("class", "emoji") .
               addAttribute ("data-emoji", kw) $
    htmlInline "span" $ Just $ htmlText t

instance (HasEmoji i, Monoid i) => HasEmoji (WithSourceMap i) where
  emoji kw t = emoji kw t <$ addName "emoji"

parseEmoji :: (Monad m, HasEmoji a) => InlineParser m a
parseEmoji = try $ do
  symbol ':'
  ts <- many1 $ satisfyWord (const True)
             <|> symbol '_'
             <|> symbol '+'
             <|> symbol '-'
  symbol ':'
  let kw = untokenize ts
  case emojiFromAlias kw of
    Nothing -> fail "emoji not found"
    Just t  -> return $! emoji kw t

data NodeTypeEmoji a
  = NodeEmoji Text Text
  deriving (Show)

instance Typeable a => NodeType NodeTypeEmoji a where
  type FromNodeType NodeTypeEmoji a = HasEmoji a
  fromNodeType = \case
    NodeEmoji kw t -> emoji kw t

instance ToPlainText (NodeTypeEmoji a) where
  toPlainText = \case
    NodeEmoji kw _ -> ":" <> kw <> ":"

instance (Typeable a, HasEmoji a) => HasEmoji (Nodes a) where
  emoji kw t = singleNode $ NodeEmoji kw t
