{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HttpReq (headers) where

import Common (pathTo, rechunkBS)
import Control.Applicative
import Criterion.Main (bench, bgroup, nf, nfIO)
import Control.DeepSeq (NFData(..))
import Criterion.Types (Benchmark)
import Network.Wai.Handler.Warp.RequestHeader (parseHeaderLines)
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.Attoparsec.ByteString.Char8 as APC
import qualified Data.ByteString.Char8 as BC
import qualified Data.Binary.Parser as BP
import qualified Data.Binary.Parser.Char8 as BPC
import Network.HTTP.Types.Version (HttpVersion, http11)

headers :: IO [Benchmark]
headers = do
  req <- BC.readFile =<< pathTo "http-request.txt"
  return [
        bench "http-req/attoparsec" $ nf (AP.parseOnly attoRequest) req
      , bench "http-req/binary-parsers" $ nf (BP.parseOnly bpRequest) req
      , bench "http-req/warp" $ nfIO (parseHeaderLines (BC.lines req))
      ]

--------------------------------------------------------------------------------

instance NFData HttpVersion where
    rnf !_ = ()

attoHeader = do
  name <- APC.takeWhile1 (APC.inClass "a-zA-Z0-9_-") <* APC.char ':' <* APC.skipSpace
  body <- attoBodyLine
  return (name, body)

attoBodyLine = APC.takeTill (\c -> c == '\r' || c == '\n') <* APC.endOfLine

attoReqLine = do
  m <- (APC.takeTill APC.isSpace <* APC.char ' ')
  (p,q) <- BC.break (=='?') <$> (APC.takeTill APC.isSpace <* APC.char ' ')
  v <- attoHttpVersion
  return (m,p,q,v)

attoHttpVersion = http11 <$ APC.string "HTTP/1.1"

attoRequest = (,) <$> (attoReqLine <* APC.endOfLine) <*> attoManyHeader

attoManyHeader = do
  c <- APC.peekChar'
  if c == '\r' || c == '\n'
    then return []

    else (:) <$> attoHeader <*> attoManyHeader

--------------------------------------------------------------------------------

bpHeader = do
  name <- BPC.takeWhile1 isHeaderChar <* BPC.char ':' <* BP.skipSpaces
  body <- bpBodyLine
  return (name, body)
  where
    isHeaderChar c = ('a' <= c && c <= 'z')
                  || ('A' <= c && c <= 'Z')
                  || ('0' <= c && c <= '0')
                  || c == '_' || c == '-'

bpBodyLine = BPC.takeTill (\c -> c == '\r' || c == '\n') <* BP.satisfy BP.isEndOfLine

bpReqLine = do
  m <- (BPC.takeTill BPC.isSpace <* BPC.char ' ')
  (p,q) <- BC.break (=='?') <$> (BPC.takeTill BPC.isSpace <* BPC.char ' ')
  v <- bpHttpVersion
  return (m,p,q,v)

bpHttpVersion = http11 <$ BP.string "HTTP/1.1"

bpRequest = (,) <$> (bpReqLine <* BP.satisfy BP.isEndOfLine) <*> bpManyHeader

bpManyHeader = do
  c <- BPC.peek
  if c == '\r' || c == '\n'
    then return []
    else (:) <$> bpHeader <*> bpManyHeader
