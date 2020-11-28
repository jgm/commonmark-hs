{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
module Commonmark.Extensions.Citations
  (   )
where
import Commonmark.Types
import Commonmark.Tokens
import Commonmark.Syntax
import Commonmark.Inlines
import Commonmark.SourceMap
import Commonmark.Html
import Control.Monad (mzero, guard, void)
import Text.Parsec
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup
#endif
