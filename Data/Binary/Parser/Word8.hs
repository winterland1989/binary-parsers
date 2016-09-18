{-# LANGUAGE BangPatterns #-}

module Data.Binary.Parser.Word8 where

import Control.Monad
import qualified Data.Binary.Get          as BG
import           Data.Binary.Get.Internal
import           Data.Binary.Parser
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Unsafe   as B
import qualified Data.ByteString.Lazy     as L
import           Data.Word
import           Prelude                  hiding (takeWhile)
import           Data.ByteString.Internal (ByteString(..), inlinePerformIO)
import Foreign.Ptr (castPtr, minusPtr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import qualified Foreign.Storable as Storable (Storable(peek))

--------------------------------------------------------------------------------

peekMaybe :: Get (Maybe Word8)
peekMaybe = do
    bs <- get
    if B.null bs then return Nothing
                 else return (Just (B.unsafeHead bs))
{-# INLINE peekMaybe #-}

peek :: Get Word8
peek = do
    ensureN 1
    bs <- get
    return (B.unsafeHead bs)
{-# INLINE peek #-}

-- | The parser @satisfy p@ succeeds for any byte for which the
-- predicate @p@ returns 'True'. Returns the byte that is actually
-- parsed.
--
-- >digit = satisfy isDigit
-- >    where isDigit w = w >= 48 && w <= 57
satisfy :: (Word8 -> Bool) -> Get Word8
satisfy pred = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if pred w then put (B.unsafeTail bs) >> return w
              else fail ("satisfy: can't satisfy word8: " ++ show w)
{-# INLINE satisfy #-}

word8 :: Word8 -> Get ()
word8 c = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if c == w then put (B.unsafeTail bs)
              else fail ("word8: can't match " ++ show c ++ " with " ++ show w)
{-# INLINE word8 #-}

anyWord8 :: Get Word8
anyWord8 = BG.getWord8
{-# INLINE anyWord8 #-}

skip :: (Word8 -> Bool) -> Get ()
skip pred = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if pred w then put (B.unsafeTail bs)
              else fail ("skip: can't skip word8: " ++ show w)
{-# INLINE skip #-}

--------------------------------------------------------------------------------

skipWords :: Int -> Get ()
skipWords n = do
    ensureN n
    bs <- get
    put (B.unsafeDrop n bs)
{-# INLINE skipWords #-}

takeTill :: (Word8 -> Bool) -> Get ByteString
takeTill pred = withInputChunks () (consumeUntil pred) B.concat (return . B.concat)
  where
    consumeUntil pred _ str =
        let (want, rest) = B.break pred str
        in if B.null rest then Left ()
                          else Right (want, rest)
{-# INLINE takeTill #-}

takeTillLazy :: (Word8 -> Bool) -> Get L.ByteString
takeTillLazy pred = withInputChunks () (consumeUntil pred) L.fromChunks (return . L.fromChunks)
  where
    consumeUntil pred _ str =
        let (want, rest) = B.break pred str
        in if B.null rest then Left ()
                          else Right (want, rest)
{-# INLINE takeTillLazy #-}

takeWhile :: (Word8 -> Bool) -> Get ByteString
takeWhile pred = withInputChunks () (consumeUntil pred) B.concat (return . B.concat)
  where
    consumeUntil pred _ str =
        let (want, rest) = B.span pred str
        in if B.null rest then Left ()
                          else Right (want, rest)
{-# INLINE takeWhile #-}

takeWhile1 :: (Word8 -> Bool) -> Get ByteString
takeWhile1 pred = do
    bs <- takeWhile pred
    if B.null bs then fail "takeWhile1" else return bs
{-# INLINE takeWhile1 #-}

takeWhileLazy :: (Word8 -> Bool) -> Get L.ByteString
takeWhileLazy pred = withInputChunks () (consumeUntil pred) L.fromChunks (return . L.fromChunks)
  where
    consumeUntil pred _ str =
        let (want, rest) = B.span pred str
        in if B.null rest then Left ()
                          else Right (want, rest)
{-# INLINE takeWhileLazy #-}

skipWhile :: (Word8 -> Bool) -> Get ()
skipWhile pred = void $ withInputChunks () (consumeUntil pred) B.concat (return . const B.empty)
  where
    consumeUntil pred _ str =
        let (_, rest) = B.span pred str
        in if B.null rest then Left ()
                          else Right (B.empty , rest)
{-# INLINE skipWhile #-}

skipTill :: (Word8 -> Bool) -> Get ()
skipTill pred = void $ withInputChunks () (consumeUntil pred) B.concat (return . const B.empty)
  where
    consumeUntil pred _ str =
        let (_, rest) = B.break pred str
        in if B.null rest then Left ()
                          else Right (B.empty, rest)
{-# INLINE skipTill #-}

skipSpaces :: Get ()
skipSpaces = skipWhile isSpace
{-# INLINE skipSpaces #-}

scan :: s -> (s -> Word8 -> Maybe s) -> Get ByteString
scan s consume = withInputChunks s consume' B.concat (return . B.concat)
  where
    consume' s0 (PS fp off len) = inlinePerformIO $
        withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
                go ptr !s
                    | ptr < end = do
                        w <- Storable.peek ptr
                        case consume s w of
                            Just s' -> go (ptr `plusPtr` 1) s'
                            _       -> do
                                let !len1 = ptr `minusPtr` start
                                    !off2 = off + len1
                                    !len2 = end `minusPtr` ptr
                                return (Right (PS fp off len1, PS fp off2 len2))
                    | otherwise = return (Left s)
            go start s0
{-# INLINE scan #-}

scanChunks :: s -> Consume s -> Get ByteString
scanChunks s consume = withInputChunks s consume B.concat (return . B.concat)
{-# INLINE scanChunks #-}

--------------------------------------------------------------------------------

-- | Fast 'Word8' predicate for matching ASCII space characters.
isSpace :: Word8 -> Bool
isSpace w = w == 32 || w - 9 <= 4
{-# INLINE isSpace #-}

-- | Digit predicate.
isDigit :: Word8 -> Bool
isDigit w = w - 48 <= 9
{-# INLINE isDigit #-}

-- | HexDigit predicate.
isHexDigit :: Word8 -> Bool
isHexDigit w = (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
{-# INLINE isHexDigit #-}

-- | A predicate that matches either a space @\' \'@ or horizontal tab
-- @\'\\t\'@ character.
isHorizontalSpace :: Word8 -> Bool
isHorizontalSpace w = w == 32 || w == 9
{-# INLINE isHorizontalSpace #-}

-- | A predicate that matches either a carriage return @\'\\r\'@ or
-- newline @\'\\n\'@ character.
isEndOfLine :: Word8 -> Bool
isEndOfLine w = w == 13 || w == 10
{-# INLINE isEndOfLine #-}
