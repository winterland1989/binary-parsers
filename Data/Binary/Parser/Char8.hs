module Data.Binary.Parser.Char8 where

import qualified Data.Binary.Get          as BG
import           Data.Binary.Get.Internal
import qualified Data.Binary.Parser.Word8 as W
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Unsafe   as B
import           Prelude                  hiding (takeWhile)

--------------------------------------------------------------------------------

peekMaybe :: Get (Maybe Char)
peekMaybe = fmap w2c <$> W.peekMaybe
{-# INLINE peekMaybe #-}

peek :: Get Char
peek = w2c <$> W.peek
{-# INLINE peek #-}

satisfy :: (Char -> Bool) -> Get Char
satisfy p = w2c <$> W.satisfy (p . w2c)
{-# INLINE satisfy #-}

char :: Char -> Get ()
char c = W.word8 (c2w c)
{-# INLINE char #-}

anyChar :: Get Char
anyChar = w2c <$> BG.getWord8
{-# INLINE anyChar #-}

skipChar :: (Char -> Bool) -> Get ()
skipChar p = W.skipWord8 (p . w2c)
{-# INLINE skipChar #-}

--------------------------------------------------------------------------------

takeTill :: (Char -> Bool) -> Get ByteString
takeTill p = W.takeTill (p . w2c)
{-# INLINE takeTill #-}

takeWhile :: (Char -> Bool) -> Get ByteString
takeWhile p = W.takeWhile (p . w2c)
{-# INLINE takeWhile #-}

takeWhile1 :: (Char -> Bool) -> Get ByteString
takeWhile1 p = W.takeWhile1 (p . w2c)
{-# INLINE takeWhile1 #-}

skipWhile :: (Char -> Bool) -> Get ()
skipWhile p = W.skipWhile (p . w2c)
{-# INLINE skipWhile #-}

skipTill :: (Char -> Bool) -> Get ()
skipTill p = W.skipTill (p . w2c)
{-# INLINE skipTill #-}

stringCI :: ByteString -> Get ByteString
stringCI bs = do
    let l = B.length bs
    ensureN l
    bs' <- B.unsafeTake l <$> get
    if B.map toLower bs' == B.map toLower bs
    then put (B.unsafeDrop l bs') >> return bs'
    else fail ("string not match: " ++ show bs)
  where
    toLower w | w >= 65 && w <= 90 = w + 32
              | otherwise          = w
{-# INLINE stringCI #-}

--------------------------------------------------------------------------------

isSpace :: Char -> Bool
isSpace c = (c == ' ') || ('\t' <= c && c <= '\r')
{-# INLINE isSpace #-}

-- | A fast digit predicate.
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'
{-# INLINE isDigit #-}

-- | HexDigit predicate.
isHexDigit :: Char -> Bool
isHexDigit c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
{-# INLINE isHexDigit #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Char -> Bool
isHorizontalSpace c = c == ' ' || c == '\t'
{-# INLINE isHorizontalSpace #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Char -> Bool
isEndOfLine c = c == '\r' || c == '\n'
{-# INLINE isEndOfLine #-}
