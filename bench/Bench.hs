{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import qualified Aeson
import qualified AesonBP

main :: IO ()
main = do
    aeson <- Aeson.aeson
    aesonbp <- AesonBP.aeson
    defaultMain [ aeson, aesonbp ]
