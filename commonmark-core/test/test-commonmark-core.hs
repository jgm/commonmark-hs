{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Commonmark
import           Commonmark.Extensions.Smart
import           Commonmark.Extensions.Strikethrough
import           Commonmark.Extensions.PipeTable
import           Commonmark.Extensions.Math
import           Commonmark.Extensions.Autolink
import           Control.Monad         (when)
import           Data.Functor.Identity
import           Data.List             (sort, groupBy)
import           Data.Monoid           ((<>))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Data.Text.Lazy        as TL
import           Lucid
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Text.HTML.TagSoup
import           Text.Parsec
import           Text.Parsec.Pos

main :: IO ()
main = do
  defaultMain $ testGroup "Tests" []
