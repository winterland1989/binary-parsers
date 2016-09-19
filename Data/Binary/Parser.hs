{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Data.Binary.Parser
-- Copyright   :  Daan Leijen 1999-2001, Bryan O'Sullivan 2007-2015, Winterland 2016
-- License     :  BSD3
--
-- Maintainer  :  drkoster@qq.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Useful parser combinators, similar to those provided by Parsec.
-- choice -> asum

module Data.Binary.Parser
    ( parseOnly
    , parseLazy
    -- *
    , (<?>)
    , endOfInput
    , atEnd
    -- *
    , option
    , eitherP
    , match
    , many'
    , some'
    , sepBy
    , sepBy'
    , sepBy1
    , sepBy1'
    , manyTill
    , manyTill'
    , skipMany
    , skipMany1
    -- *
    , module Data.Binary.Get
    , module Data.Binary.Parser.Word8
    , module Data.Binary.Parser.Numeric
    ) where

import Data.Binary.Get
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import Data.Binary.Parser.Word8
import Data.Binary.Parser.Numeric

--------------------------------------------------------------------------------

parseOnly :: Get a -> B.ByteString -> Either String a
parseOnly g bs =
    let d = runGetIncremental g
    in case pushEndOfInput (pushChunk d bs) of
        Fail _ _ err -> Left err
        Done _ _ a -> Right a
        _ -> Left "parseOnly: impossible error!"
{-# INLINE parseOnly #-}

parseLazy :: Get a -> L.ByteString -> Either String a
parseLazy g lbs =
    let d = runGetIncremental g
    in case pushEndOfInput (pushChunks d lbs) of
        Fail _ _ err -> Left err
        Done _ _ a -> Right a
        _ -> Left "parseOnly: impossible error!"
{-# INLINE parseLazy #-}

--------------------------------------------------------------------------------

(<?>) :: Get a -> String -> Get a
(<?>) = flip label
infix 0 <?>
{-# INLINE (<?>) #-}

-- | alias for 'isEmpty'
atEnd :: Get Bool
atEnd = isEmpty

-- | Match only if all input has been consumed.
endOfInput :: Get ()
endOfInput = do
    e <- atEnd
    unless e (fail "endOfInput: not meet end yet")

-- | @option x p@ tries to apply action @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value
-- returned by @p@.
--
-- > priority  = option 0 (digitToInt <$> digit)
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x
{-# SPECIALIZE option :: a -> Get a -> Get a #-}

-- | Combine two alternatives.
eitherP :: (Alternative f) => f a -> f b -> f (Either a b)
eitherP a b = (Left <$> a) <|> (Right <$> b)
{-# INLINE eitherP #-}

-- | Return both the result of a parse and the portion of the input
-- that was consumed while it was being parsed.
match :: Get a -> Get (B.ByteString, a)
match p = do
    pos1 <- bytesRead
    (x, pos2) <- lookAhead $ (,) <$> p <*> bytesRead
    (,) <$> (getByteString . fromIntegral) (pos2 - pos1) <*> pure x

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | @many' p@ applies the action @p@ /zero/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many' letter
many' :: (MonadPlus m) => m a -> m [a]
many' p = many_p
  where many_p = some_p `mplus` return []
        some_p = liftM2' (:) p many_p
{-# INLINE many' #-}

-- | @many1' p@ applies the action @p@ /one/ or more times. Returns a
-- list of the returned values of @p@. The value returned by @p@ is
-- forced to WHNF.
--
-- >  word  = many1' letter
some' :: (MonadPlus m) => m a -> m [a]
some' p = liftM2' (:) p (many' p)
{-# INLINE some' #-}

-- | @sepBy p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy` (char ',')
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p ((s *> sepBy1 p s) <|> pure []) <|> pure []
{-# SPECIALIZE sepBy :: Get a -> Get s -> Get [a] #-}

-- | @sepBy' p sep@ applies /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy'` (char ',')
sepBy' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy' p s = go `mplus` return []
  where go = liftM2' (:) p ((s >> sepBy1' p s) `mplus` return [])
{-# SPECIALIZE sepBy' :: Get a -> Get s -> Get [a] #-}

-- | @sepBy1 p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@.
--
-- > commaSep p  = p `sepBy1` (char ',')
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = go
    where go = liftA2 (:) p ((s *> go) <|> pure [])
{-# SPECIALIZE sepBy1 :: Get a -> Get s -> Get [a] #-}

-- | @sepBy1' p sep@ applies /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of the values returned by @p@. The value
-- returned by @p@ is forced to WHNF.
--
-- > commaSep p  = p `sepBy1'` (char ',')
sepBy1' :: (MonadPlus m) => m a -> m s -> m [a]
sepBy1' p s = go
    where go = liftM2' (:) p ((s >> go) `mplus` return [])
{-# SPECIALIZE sepBy1' :: Get a -> Get s -> Get [a] #-}

-- | @manyTill p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill anyChar (string "-->")
--
-- (Note the overlapping parsers @anyChar@ and @string \"-->\"@.
-- While this will work, it is not very efficient, as it will cause a
-- lot of backtracking.)
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = go
    where go = (end *> pure []) <|> liftA2 (:) p go
{-# SPECIALIZE manyTill :: Get a -> Get b -> Get [a] #-}

-- | @manyTill' p end@ applies action @p@ /zero/ or more times until
-- action @end@ succeeds, and returns the list of values returned by
-- @p@.  This can be used to scan comments:
--
-- >  simpleComment   = string "<!--" *> manyTill' anyChar (string "-->")
--
-- (Note the overlapping parsers @anyChar@ and @string \"-->\"@.
-- While this will work, it is not very efficient, as it will cause a
-- lot of backtracking.)
--
-- The value returned by @p@ is forced to WHNF.
manyTill' :: (MonadPlus m) => m a -> m b -> m [a]
manyTill' p end = go
    where go = (end >> return []) `mplus` liftM2' (:) p go
{-# SPECIALIZE manyTill' :: Get a -> Get b -> Get [a] #-}

-- | Skip zero or more instances of an action.
skipMany :: Alternative f => f a -> f ()
skipMany p = go
    where go = (p *> go) <|> pure ()
{-# SPECIALIZE skipMany :: Get a -> Get () #-}

-- | Skip one or more instances of an action.
skipMany1 :: Alternative f => f a -> f ()
skipMany1 p = p *> skipMany p
{-# SPECIALIZE skipMany1 :: Get a -> Get () #-}
