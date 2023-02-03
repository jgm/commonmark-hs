{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Commonmark.HtmlMod
  ( HtmlMod (..)
  , runHtmlMod
  , HtmlModifiers (..)
  -- * Helpers for defining instances
  , withHtmlMod
  )
where
import           Commonmark.Html
import           Commonmark.Types
import           Control.Monad.Trans.Reader (Reader, ReaderT (..), asks, runReader)
import           Data.Text (Text)

-- | A type for modifying HTML rendering.
--
-- Usage:
--
-- > -- change strong elements from "<strong>" to "<b>"
-- > let mods = mempty{onStrong = \_ -> htmlInline "b" . Just}
-- > runHtmlMod mods <$> commonmark fp text
newtype HtmlMod a = HtmlMod
  { unHtmlMod :: Reader (HtmlModifiers a) (Html a)
  }

runHtmlMod :: HtmlModifiers a -> HtmlMod a -> Html a
runHtmlMod mods = (`runReader` mods) . unHtmlMod

instance Show (HtmlMod a) where
  show = show . runHtmlMod mempty

instance Semigroup (HtmlMod a) where
  HtmlMod mod1 <> HtmlMod mod2 =
    HtmlMod . ReaderT $ \r ->
      pure $ runReader mod1 r <> runReader mod2 r

instance Monoid (HtmlMod a) where
  mempty = HtmlMod $ pure mempty

instance Rangeable (Html a) => Rangeable (HtmlMod a) where
  ranged range a = withHtmlMod $ \mods -> ranged range (runHtmlMod mods a)

instance HasAttributes (HtmlMod a) where
  addAttributes attrs a = withHtmlMod $ \mods -> addAttributes attrs (runHtmlMod mods a)

instance ToPlainText (HtmlMod a) where
  toPlainText a = toPlainText $ runHtmlMod mempty a

instance Rangeable (Html a) => IsInline (HtmlMod a) where
  lineBreak = withHtmlMod $ \mods -> onLineBreak mods lineBreak
  softBreak = withHtmlMod $ \mods -> onSoftBreak mods softBreak
  str t = withHtmlMod $ \mods -> onStr mods str t
  entity t = withHtmlMod $ \mods -> onEntity mods entity t
  escapedChar c = withHtmlMod $ \mods -> onEscapedChar mods escapedChar c
  emph a = withHtmlMod $ \mods -> onEmph mods emph (runHtmlMod mods a)
  strong a = withHtmlMod $ \mods -> onStrong mods strong (runHtmlMod mods a)
  link dest title a = withHtmlMod $ \mods -> onLink mods link dest title (runHtmlMod mods a)
  image src title a = withHtmlMod $ \mods -> onImage mods image src title (runHtmlMod mods a)
  code t = withHtmlMod $ \mods -> onCode mods code t
  rawInline fmt t = withHtmlMod $ \mods -> onRawInline mods rawInline fmt t

instance Rangeable (Html a) => IsBlock (HtmlMod a) (HtmlMod a) where
  paragraph il = withHtmlMod $ \mods -> onParagraph mods paragraph (runHtmlMod mods il)
  plain il = withHtmlMod $ \mods -> onPlain mods plain (runHtmlMod mods il)
  thematicBreak = withHtmlMod $ \mods -> onThematicBreak mods thematicBreak
  blockQuote b = withHtmlMod $ \mods -> onBlockQuote mods blockQuote (runHtmlMod mods b)
  codeBlock info t = withHtmlMod $ \mods -> onCodeBlock mods codeBlock info t
  heading level il = withHtmlMod $ \mods -> onHeading mods heading level (runHtmlMod mods il)
  rawBlock fmt t = withHtmlMod $ \mods -> onRawBlock mods rawBlock fmt t
  referenceLinkDefinition label dest = withHtmlMod $ \mods -> onReferenceLinkDefinition mods referenceLinkDefinition label dest
  list lType lSpacing items = withHtmlMod $ \mods -> onList mods list lType lSpacing (map (runHtmlMod mods) items)

withHtmlMod :: (HtmlModifiers a -> Html a) -> HtmlMod a
withHtmlMod f = HtmlMod $ asks f

data HtmlModifiers a = HtmlModifiers
  { onLineBreak :: Html a -> Html a
  , onSoftBreak :: Html a -> Html a
  , onStr :: (Text -> Html a) -> (Text -> Html a)
  , onEntity :: (Text -> Html a) -> (Text -> Html a)
  , onEscapedChar :: (Char -> Html a) -> (Char -> Html a)
  , onEmph :: (Html a -> Html a) -> (Html a -> Html a)
  , onStrong :: (Html a -> Html a) -> (Html a -> Html a)
  , onLink :: (Text -> Text -> Html a -> Html a) -> (Text -> Text -> Html a -> Html a)
  , onImage :: (Text -> Text -> Html a -> Html a) -> (Text -> Text -> Html a -> Html a)
  , onCode :: (Text -> Html a) -> (Text -> Html a)
  , onRawInline :: (Format -> Text -> Html a) -> (Format -> Text -> Html a)
  , onParagraph :: (Html a -> Html a) -> (Html a -> Html a)
  , onPlain :: (Html a -> Html a) -> (Html a -> Html a)
  , onThematicBreak :: Html a -> Html a
  , onBlockQuote :: (Html a -> Html a) -> (Html a -> Html a)
  , onCodeBlock :: (Text -> Text -> Html a) -> (Text -> Text -> Html a)
  , onHeading :: (Int -> Html a -> Html a) -> (Int -> Html a -> Html a)
  , onRawBlock :: (Format -> Text -> Html a) -> (Format -> Text -> Html a)
  , onReferenceLinkDefinition :: (Text -> (Text, Text) -> Html a) -> (Text -> (Text, Text) -> Html a)
  , onList :: (ListType -> ListSpacing -> [Html a] -> Html a) -> (ListType -> ListSpacing -> [Html a] -> Html a)
  }

instance Semigroup (HtmlModifiers a) where
  mod1 <> mod2 =
    HtmlModifiers
      { onLineBreak = onLineBreak mod1 . onLineBreak mod2
      , onSoftBreak = onSoftBreak mod1 . onSoftBreak mod2
      , onStr = onStr mod1 . onStr mod2
      , onEntity = onEntity mod1 . onEntity mod2
      , onEscapedChar = onEscapedChar mod1 . onEscapedChar mod2
      , onEmph = onEmph mod1 . onEmph mod2
      , onStrong = onStrong mod1 . onStrong mod2
      , onLink = onLink mod1 . onLink mod2
      , onImage = onImage mod1 . onImage mod2
      , onCode = onCode mod1 . onCode mod2
      , onRawInline = onRawInline mod1 . onRawInline mod2
      , onParagraph = onParagraph mod1 . onParagraph mod2
      , onPlain = onPlain mod1 . onPlain mod2
      , onThematicBreak = onThematicBreak mod1 . onThematicBreak mod2
      , onBlockQuote = onBlockQuote mod1 . onBlockQuote mod2
      , onCodeBlock = onCodeBlock mod1 . onCodeBlock mod2
      , onHeading = onHeading mod1 . onHeading mod2
      , onRawBlock = onRawBlock mod1 . onRawBlock mod2
      , onReferenceLinkDefinition = onReferenceLinkDefinition mod1 . onReferenceLinkDefinition mod2
      , onList = onList mod1 . onList mod2
      }

instance Monoid (HtmlModifiers a) where
  mempty =
    HtmlModifiers
      { onLineBreak = id
      , onSoftBreak = id
      , onStr = id
      , onEntity = id
      , onEscapedChar = id
      , onEmph = id
      , onStrong = id
      , onLink = id
      , onImage = id
      , onCode = id
      , onRawInline = id
      , onParagraph = id
      , onPlain = id
      , onThematicBreak = id
      , onBlockQuote = id
      , onCodeBlock = id
      , onHeading = id
      , onRawBlock = id
      , onReferenceLinkDefinition = id
      , onList = id
      }

