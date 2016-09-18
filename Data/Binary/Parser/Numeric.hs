{-# LANGUAGE BangPatterns #-}

module Data.Binary.Parser.Numeric where

import Control.Applicative
import Control.Monad
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
import           Data.Int
import           Data.Bits
import qualified Data.ByteString.Lex.Integral as LexInt
import           Data.Scientific (Scientific(..))
import qualified Data.Scientific as Sci

-- | Parse and decode an unsigned hexadecimal number.  The hex digits
-- @\'a\'@ through @\'f\'@ may be upper or lower case.
--
-- This parser does not accept a leading @\"0x\"@ string.
hexadecimal :: (Integral a, Bits a) => Get a
hexadecimal = do
    bs <- W.takeWhile1 W.isHexDigit
    case LexInt.readHexadecimal bs of
        Just (x, _) -> return x
        Nothing -> fail "decimal: impossible"
{-# SPECIALISE hexadecimal :: Get Int #-}
{-# SPECIALISE hexadecimal :: Get Int8 #-}
{-# SPECIALISE hexadecimal :: Get Int16 #-}
{-# SPECIALISE hexadecimal :: Get Int32 #-}
{-# SPECIALISE hexadecimal :: Get Int64 #-}
{-# SPECIALISE hexadecimal :: Get Integer #-}
{-# SPECIALISE hexadecimal :: Get Word #-}
{-# SPECIALISE hexadecimal :: Get Word8 #-}
{-# SPECIALISE hexadecimal :: Get Word16 #-}
{-# SPECIALISE hexadecimal :: Get Word32 #-}
{-# SPECIALISE hexadecimal :: Get Word64 #-}

-- | Parse and decode an unsigned decimal number.
decimal :: Integral a => Get a
decimal = do
    bs <- W.takeWhile1 W.isDigit
    case LexInt.readDecimal bs of
        Just (x, _) -> return x
        Nothing -> fail "decimal: impossible"
{-# SPECIALISE decimal :: Get Int #-}
{-# SPECIALISE decimal :: Get Int8 #-}
{-# SPECIALISE decimal :: Get Int16 #-}
{-# SPECIALISE decimal :: Get Int32 #-}
{-# SPECIALISE decimal :: Get Int64 #-}
{-# SPECIALISE decimal :: Get Integer #-}
{-# SPECIALISE decimal :: Get Word #-}
{-# SPECIALISE decimal :: Get Word8 #-}
{-# SPECIALISE decimal :: Get Word16 #-}
{-# SPECIALISE decimal :: Get Word32 #-}
{-# SPECIALISE decimal :: Get Word64 #-}

-- | Parse a number with an optional leading @\'+\'@ or @\'-\'@ sign
-- character.
signed :: Num a => Get a -> Get a
signed p = (negate <$> (W.word8 minus *> p))
       <|> (W.word8 plus *> p)
       <|> p
  where
    minus = 45     -- '-'
    plus  = 43     -- '+'
{-# SPECIALISE signed :: Get Int -> Get Int #-}
{-# SPECIALISE signed :: Get Int8 -> Get Int8 #-}
{-# SPECIALISE signed :: Get Int16 -> Get Int16 #-}
{-# SPECIALISE signed :: Get Int32 -> Get Int32 #-}
{-# SPECIALISE signed :: Get Int64 -> Get Int64 #-}
{-# SPECIALISE signed :: Get Integer -> Get Integer #-}

-- | Parse a rational number.
--
-- The syntax accepted by this parser is the same as for 'double'.
--
-- /Note/: this parser is not safe for use with inputs from untrusted
-- sources.  An input with a suitably large exponent such as
-- @"1e1000000000"@ will cause a huge 'Integer' to be allocated,
-- resulting in what is effectively a denial-of-service attack.
--
-- In most cases, it is better to use 'double' or 'scientific'
-- instead.
rational :: Fractional a => Get a
rational = scientifically realToFrac
{-# SPECIALIZE rational :: Get Double #-}
{-# SPECIALIZE rational :: Get Float #-}
{-# SPECIALIZE rational :: Get Rational #-}
{-# SPECIALIZE rational :: Get Scientific #-}

-- | Parse a rational number.
--
-- This parser accepts an optional leading sign character, followed by
-- at least one decimal digit.  The syntax similar to that accepted by
-- the 'read' function, with the exception that a trailing @\'.\'@ or
-- @\'e\'@ /not/ followed by a number is not consumed.
--
-- Examples with behaviour identical to 'read', if you feed an empty
-- continuation to the first result:
--
-- >double "3"     == Done 3.0 ""
-- >double "3.1"   == Done 3.1 ""
-- >double "3e4"   == Done 30000.0 ""
-- >double "3.1e4" == Done 31000.0, ""
--
-- Examples with behaviour identical to 'read':
--
-- >double ".3"    == Fail "input does not start with a digit"
-- >double "e3"    == Fail "input does not start with a digit"
--
-- Examples of differences from 'read':
--
-- >double "3.foo" == Done 3.0 ".foo"
-- >double "3e"    == Done 3.0 "e"
--
-- This function does not accept string representations of \"NaN\" or
-- \"Infinity\".
double :: Get Double
double = scientifically Sci.toRealFloat

scientific :: Get Scientific
scientific = scientifically id

data SP = SP !Integer {-# UNPACK #-}!Int

scientifically :: (Scientific -> a) -> Get a
scientifically h = do
    sign <- W.peek
    when (sign == plus || sign == minus) (W.skipN 1)

    n <- decimal

    maybeDot <- W.peekMaybe
    SP c e <- if maybeDot == Just dot
                    then W.skipN 1 *> (mkFrac n <$> W.takeWhile W.isDigit)
                    else pure (SP n 0)

    let signedCoeff | sign /= minus = c
                    | otherwise = negate c

    (do W.satisfy (\ex -> ex == littleE || ex == bigE)
        h . Sci.scientific signedCoeff . (e +) <$> signed decimal)
        <|> (return $! h (Sci.scientific signedCoeff e))

  where
    minus = 45     -- '-'
    plus  = 43     -- '+'
    littleE = 101  -- 'e'
    bigE    = 69   -- 'E'
    dot = 46       -- '.'
    mkFrac intDigits fracDigits = SP (B.foldl' step intDigits fracDigits) (negate $ B.length fracDigits)
    step a w = a * 10 + fromIntegral (w - 48)
{-# INLINE scientifically #-}
