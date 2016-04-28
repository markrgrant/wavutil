{-# LANGUAGE OverloadedStrings #-}
module WAV.Parser where

import WAV
import Prelude hiding             (take, count)
import Data.Word                  (Word32, Word8)
import Data.Attoparsec.ByteString (Parser, take, string, anyWord8,
                                   manyTill, parseOnly, Result, count, choice,
                                   many')
import Data.Binary.Get            (runGet, getWord16le, getWord32le, getWord8)
import Data.Binary.Put            (runPut, putWord32le)
import Control.Monad              (void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L


parseFromFile :: FilePath -> IO (Either String RIFF)
parseFromFile fp =
  B.readFile fp >>= \contents -> 
  return $ parseOnly riffParser contents


riffParser :: Parser RIFF
riffParser = do
    _        <- fourccParser
    length   <- lengthParser
    riffdata <- riffDataParser
    return $ RIFF riffdata


riffDataParser :: Parser RIFFData
riffDataParser = choice [waveParser, otherRIFFParser]


-- the RIFF contains WAV data
waveParser :: Parser RIFFData
waveParser = do
  _ <- string "WAVE"
  chunks <- many' $ choice [fmtParser, dataParser, otherChunkParser]
  return $ WaveRIFF chunks


otherRIFFParser :: Parser RIFFData
otherRIFFParser = do
  formtype <- take 4
  chunks <- many' otherChunkParser
  return $ OtherRIFF formtype


fmtParser :: Parser WaveChunk
fmtParser = do
  _ <- string "fmt "
  info <- fmtInfo16Parser
  return $ FmtChunk info


fmtInfo16Parser :: Parser FmtInfo
fmtInfo16Parser = do
  let bs16le = L.toStrict (runPut (putWord32le (16::Word32)))
  _ <- string bs16le
  wft <- fmtCodeParser
  nch <- twoByteParser
  sps <- lengthParser
  bps <- lengthParser
  ba  <- twoByteParser
  bp  <- twoByteParser
  return FmtInfo16 {
    wFormatTag=wft,
    nChannels=nch,
    nSamplesPerSec=sps,
    nAvgBytesPerSec=bps,
    nBlockAlign=ba,
    wBitsPerSample=bp}


fmtCodeParser :: Parser FmtCode
fmtCodeParser = do
  code <- twoByteParser
  case code of
    1 -> return PCM
    3 -> return IEEEFloat
    6 -> return ALAW
    7 -> return MULAW
    65534 -> return EXTENSIBLE

dataParser :: Parser WaveChunk
dataParser = do
  _ <- string "data"
  length <- lengthParser
  dat <- take length
  return $ DataChunk dat


otherChunkParser :: Parser WaveChunk
otherChunkParser = do
    fourcc <- take 4
    length <- lengthParser
    dat    <- take length
    return $ OtherChunk fourcc dat


fourccParser :: Parser ()
fourccParser = void $ string "RIFF"


lengthParser :: Parser Int
lengthParser = do
    str <- take 4
    let word = runGet getWord32le (L.fromStrict str)
    return $ fromIntegral word


twoByteParser :: Parser Int
twoByteParser = do
    str <- take 2
    let word = runGet getWord16le (L.fromStrict str)
    return $ fromIntegral word
