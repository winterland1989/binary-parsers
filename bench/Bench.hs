{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import qualified Aeson
import qualified AesonBP
import qualified HttpReq
import Data.List

main :: IO ()
main = do
    http <- HttpReq.headers
    defaultMain http

    aeson <- Aeson.aeson
    aesonbp <- AesonBP.aeson
    aesonLazy <- Aeson.aesonLazy
    aesonbpLazy <- AesonBP.aesonLazy
    (defaultMain . concat . transpose) [ aeson, aesonbp, aesonLazy, aesonbpLazy ]
