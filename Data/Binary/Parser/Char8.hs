module Data.Binary.Parser.Char8 where

import qualified Data.Binary.Get          as BG
import           Data.Binary.Get.Internal
import           Data.Binary.Parser
import qualified        Data.Binary.Parser.Word8 as W
import           Data.ByteString          (ByteString)
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Unsafe   as B
import qualified Data.ByteString.Lazy     as L
import           Data.Word
import           Prelude                  hiding (takeWhile)


--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

peekMaybe :: Get (Maybe Char)
peekMaybe = fmap w2c <$> W.peekMaybe
{-# INLINE peekMaybe #-}

peek :: Get Char
peek = w2c <$> W.peek
{-# INLINE peek #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Char -> Bool) -> Get Char
satisfy pred = w2c <$> W.satisfy (pred . w2c)
{-# INLINE satisfy #-}

char :: Char -> Get ()
char c = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if c2w c == w then put (B.unsafeTail bs)
                  else fail ("word8: can't get char: " ++ show c)
{-# INLINE char #-}

anyChar :: Get Char
anyChar = w2c <$> BG.getWord8
{-# INLINE anyChar #-}

skip :: (Char -> Bool) -> Get ()
skip pred = W.skip (pred . w2c)
{-# INLINE skip #-}

--------------------------------------------------------------------------------

takeTill :: (Char -> Bool) -> Get ByteString
takeTill pred = W.takeTill (pred . w2c)
{-# INLINE takeTill #-}

takeTillLazy :: (Char -> Bool) -> Get L.ByteString
takeTillLazy pred = W.takeTillLazy (pred . w2c)
{-# INLINE takeTillLazy #-}

takeWhile :: (Char -> Bool) -> Get ByteString
takeWhile pred = W.takeWhile (pred . w2c)
{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Get ByteString
takeWhile1 pred = W.takeWhile1 (pred . w2c)
{-# INLINE takeWhile1 #-}

takeWhileLazy :: (Char -> Bool) -> Get L.ByteString
takeWhileLazy pred = W.takeWhileLazy (pred . w2c)
{-# INLINE takeWhileLazy #-}

skipWhile :: (Char -> Bool) -> Get ()
skipWhile pred = W.skipWhile (pred . w2c)
{-# INLINE skipWhile #-}

skipTill :: (Char -> Bool) -> Get ()
skipTill pred = W.skipTill (pred . w2c)
{-# INLINE skipTill #-}

--------------------------------------------------------------------------------

string :: ByteString -> Get ()
string bs = do
    let l = B.length bs
    ensureN l
    bs' <- get
    if B.unsafeTake l bs' == bs
    then put (B.unsafeDrop l bs')
    else fail ("string not match: " ++ show bs)
{-# INLINE string #-}

stringCI :: ByteString -> Get ()
stringCI bs = do
    let l = B.length bs
    ensureN l
    bs' <- get
    if B.map toLower (B.unsafeTake l bs') == B.map toLower bs
    then put (B.unsafeDrop l bs')
    else fail ("string not match: " ++ show bs)
  where
    toLower w | w >= 65 && w <= 90 = w + 32
              | otherwise          = w
{-# INLINE stringCI #-}

skipSpaces :: Get ()
skipSpaces = W.skipSpaces
{-# INLINE skipSpaces #-}

--------------------------------------------------------------------------------

isSpace :: Char -> Bool
isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

-- | A fast digit predicate.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}
