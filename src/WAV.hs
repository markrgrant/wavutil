module WAV where

import Data.Binary
import qualified Data.ByteString as B
import Data.Binary.Get
import System.Environment


type Length    = Int
type FormType  = B.ByteString
type FourCC    = B.ByteString
data RIFF      = RIFF RIFFData deriving (Show)
data RIFFData  = WaveRIFF   [WaveChunk]
               | OtherRIFF  FormType deriving (Show)
data WaveChunk = FmtChunk   FmtInfo
               | DataChunk  B.ByteString
               | OtherChunk FourCC B.ByteString deriving (Eq, Show)
data FmtInfo   = FmtInfo16 {
                   wFormatTag :: FmtCode,
                   nChannels  :: Int,
                   nSamplesPerSec :: Int,
                   nAvgBytesPerSec :: Int,
                   nBlockAlign :: Int,
                   wBitsPerSample :: Int
                 } deriving (Eq, Show)
data FmtCode   = PCM
               | IEEEFloat
               | ALAW
               | MULAW
               | EXTENSIBLE deriving (Eq, Show)

instance Binary RIFF where
  get = undefined
  put (RIFF riffdata) = 
    put "RIFF" >>
    put (riffDataSize riffdata) >>
    put riffdata


riffDataSize :: RIFFData -> Int
riffDataSize (WaveRIFF chunks) = 4 + sum (map waveChunkSize chunks)
riffDataSize _ = undefined


waveChunkSize :: WaveChunk -> Int
waveChunkSize (FmtChunk _) = 24
waveChunkSize (DataChunk dat) = 4 + B.length dat
waveChunkSize _ = undefined


instance Binary RIFFData where
  get = undefined
  put (WaveRIFF chunks) =
    put "WAVE" >> put chunks
  put (OtherRIFF formtype) = undefined


instance Binary WaveChunk where
  get = undefined
  put (FmtChunk fmtinfo) = 
    put "fmt " >>
    put fmtinfo
  put (DataChunk dat) =
    put "data" >>
    put (B.length dat) >>
    put dat
  put (OtherChunk cc dat) = undefined


instance Binary FmtInfo where
  get = undefined
  put f =
    put (16 :: Word16) >>
    put (wFormatTag f) >>
    put (fromIntegral (nChannels f) :: Word16) >>
    put (fromIntegral (nSamplesPerSec f) :: Word32) >>
    put (fromIntegral (nAvgBytesPerSec f) :: Word32) >>
    put (fromIntegral (nBlockAlign f) :: Word16) >>
    put (fromIntegral (wBitsPerSample f) :: Word16)
  

instance Binary FmtCode where
  get            = undefined
  put PCM        = put (1 :: Word16)
  put IEEEFloat  = put (3 :: Word16)
  put ALAW       = put (6 :: Word16)
  put MULAW      = put (7 :: Word16)
  put EXTENSIBLE = put (65534 :: Word16)
