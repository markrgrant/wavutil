{-# LANGUAGE OverloadedStrings #-}
module WAV.Parser.Test where

import WAV.Parser
import WAV
import Data.Attoparsec.ByteString
import Data.ByteString.Lazy as L
import Data.Binary.Put (putWord16le, putWord32le, runPut, putByteString)
import Data.Word (Word32)

{-
testRIFFParser :: Bool
testRIFFParser =
  let riff = 
      riffbs = 
  maybeResult (parse riffParser riffbs) = Just testriff
-}

testfourccParser :: Bool
testfourccParser = maybeResult (parse fourccParser "RIFF") == Just ()


testLengthParser :: Bool
testLengthParser =
  let bs42 = L.toStrict (runPut (putWord32le (42::Word32)))
  in maybeResult (parse lengthParser bs42) == Just 42


testFmtParser :: Bool
testFmtParser = 
  let fmt = L.toStrict (runPut (putByteString "fmt " >>
       putWord32le 16 >>
       putWord16le 1  >> 
       putWord32le 0  >>
       putWord32le 0  >>
       putWord16le 0  >>
       putWord16le 0))
  in maybeResult (parse fmtParser fmt) == Just (FmtChunk FmtInfo16 {
    wFormatTag=PCM,
    nChannels=0,
    nSamplesPerSec=0,
    nAvgBytesPerSec=0,
    nBlockAlign=0,
    wBitsPerSample=0})


testDataParser :: Bool
testDataParser = 
  let fmt = L.toStrict (runPut (putByteString "data" >> putWord32le (4::Word32) >> putByteString "abcd"))
  in maybeResult (parse dataParser fmt) == Just (DataChunk "abcd")
