{-# LANGUAGE BangPatterns #-}

module Data.Binary.Parser
    ( (<?>)
    , runGet
    , parseOnly
    ) where

import Data.Binary.Get
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B

parseOnly :: Get a -> B.ByteString -> Either String a
parseOnly g bs =
    let d = runGetIncremental g
    in case pushEndOfInput (pushChunk d bs) of
        Fail _ _ err -> Left err
        Done _ _ a -> Right a
        _ -> Left "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

(<?>) :: Get a -> String -> Get a
(<?>) = flip label
infix 0 <?>
{-# INLINE (<?>) #-}
