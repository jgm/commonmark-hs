{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Tasty
-- import           Test.Tasty.HUnit
-- import           Test.Tasty.QuickCheck

main :: IO ()
main = do
  defaultMain $ testGroup "Tests" []
