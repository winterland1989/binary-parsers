{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Main (main) where

import qualified QC.ByteString as ByteString
import qualified QC.Combinator as Combinator
import Test.Tasty (defaultMain, testGroup)

main = defaultMain tests

tests = testGroup "tests" [
    testGroup "bs" ByteString.tests
  , testGroup "combinator" Combinator.tests
  ]
