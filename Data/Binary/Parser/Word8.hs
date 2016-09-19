{-# LANGUAGE BangPatterns #-}

module Data.Binary.Parser.Word8 where

import           Control.Monad
import           Data.Binary.Get
import           Data.Binary.Get.Internal
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.ByteString.Internal (ByteString (..), accursedUnutterablePerformIO)
import qualified Data.ByteString.Unsafe   as B
import           Data.Word
import           Foreign.ForeignPtr       (withForeignPtr)
import           Foreign.Ptr              (minusPtr, plusPtr)
import qualified Foreign.Storable         as Storable (Storable (peek))
import           Prelude                  hiding (takeWhile)

--------------------------------------------------------------------------------

peekMaybe :: Get (Maybe Word8)
peekMaybe = do
    e <- isEmpty
    if e then return Nothing
         else Just <$> peek
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
satisfy p = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if p w then put (B.unsafeTail bs) >> return w
              else fail "satisfy"
{-# INLINE satisfy #-}

satisfyWith :: (Word8 -> a) -> (a -> Bool) -> Get a
satisfyWith f p = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
        r = f w
    if p r then put (B.unsafeTail bs) >> return r
           else fail "satisfyWith"
{-# INLINE satisfyWith #-}

word8 :: Word8 -> Get ()
word8 c = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if c == w then put (B.unsafeTail bs)
              else fail "word8"
{-# INLINE word8 #-}

anyWord8 :: Get Word8
anyWord8 = getWord8
{-# INLINE anyWord8 #-}

skipWord8 :: (Word8 -> Bool) -> Get ()
skipWord8 p = do
    ensureN 1
    bs <- get
    let w = B.unsafeHead bs
    if p w then put (B.unsafeTail bs)
              else fail "skip"
{-# INLINE skipWord8 #-}

--------------------------------------------------------------------------------

skipN :: Int -> Get ()
skipN n = do
    bs <- get
    let l = B.length bs
    if l >= n then put (B.unsafeDrop n bs)
              else put B.empty >> skip (l - n)
{-# INLINE skipN #-}

takeTill :: (Word8 -> Bool) -> Get ByteString
takeTill p = do
    bs <- get
    let (want, rest) = B.break p bs
    put rest
    if B.null rest then B.concat . reverse <$> go [want]
                   else return want
  where
    go acc = do
        bs <- get
        let (want, rest) = B.break p bs
            acc' = want : acc
        put rest
        if B.null rest
        then do
            e <- isEmpty
            if e then return acc' else go acc'
        else return acc'
{-# INLINE takeTill #-}

takeWhile :: (Word8 -> Bool) -> Get ByteString
takeWhile p = do
    bs <- get
    let (want, rest) = B.span p bs
    put rest
    if B.null rest then B.concat . reverse <$> go [want]
                   else return want
  where
    go acc = do
        bs <- get
        let (want, rest) = B.span p bs
            acc' = want : acc
        put rest
        if B.null rest
        then do
            e <- isEmpty
            if e then return acc' else go acc'
        else return acc'
{-# INLINE takeWhile #-}

takeWhile1 :: (Word8 -> Bool) -> Get ByteString
takeWhile1 p = do
    bs <- takeWhile p
    if B.null bs then fail "takeWhile1" else return bs
{-# INLINE takeWhile1 #-}

skipWhile :: (Word8 -> Bool) -> Get ()
skipWhile p = do
    bs <- get
    let rest = B.dropWhile p bs
    put rest
    when (B.null rest) go
  where
    go = do
        e <- isEmpty
        unless e $ do
            bs <- get
            let rest = B.dropWhile p bs
            put rest
            when (B.null rest) go
{-# INLINE skipWhile #-}

skipTill :: (Word8 -> Bool) -> Get ()
skipTill p = skipWhile (not . p)
{-# INLINE skipTill #-}

skipSpaces :: Get ()
skipSpaces = skipWhile isSpace
{-# INLINE skipSpaces #-}

string :: ByteString -> Get ()
string bs = do
    let l = B.length bs
    ensureN l
    bs' <- get
    if B.unsafeTake l bs' == bs
    then put (B.unsafeDrop l bs')
    else fail ("string not match: " ++ show bs)
{-# INLINE string #-}

scan :: s -> (s -> Word8 -> Maybe s) -> Get ByteString
scan s0 consume = withInputChunks s0 consume' B.concat (return . B.concat)
  where
    consume' s1 (PS fp off len) = accursedUnutterablePerformIO $
        withForeignPtr fp $ \ptr0 -> do
            let start = ptr0 `plusPtr` off
                end   = start `plusPtr` len
            go fp off start end start s1
    go fp off start end ptr !s
        | ptr < end = do
            w <- Storable.peek ptr
            case consume s w of
                Just s' -> go fp off start end (ptr `plusPtr` 1) s'
                _       -> do
                    let !len1 = ptr `minusPtr` start
                        !off2 = off + len1
                        !len2 = end `minusPtr` ptr
                    return (Right (PS fp off len1, PS fp off2 len2))
        | otherwise = return (Left s)
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
