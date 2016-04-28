module WAV where


import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Binary
import Data.Binary.Get
import System.Environment


type Length    = Int

type FormType  = B.ByteString

type FourCC    = B.ByteString

data RIFF      = RIFF RIFFData

data RIFFData  = WaveRIFF   [WaveChunk]
               | OtherRIFF  FormType

data WaveChunk = FmtChunk   FmtInfo
               | DataChunk  B.ByteString
               | OtherChunk FourCC B.ByteString deriving (Eq)

data FmtInfo   = FmtInfo16 {
                   wFormatTag :: FmtCode,
                   nChannels  :: Int,
                   nSamplesPerSec :: Int,
                   nAvgBytesPerSec :: Int,
                   nBlockAlign :: Int,
                   wBitsPerSample :: Int
                 } deriving (Eq)

data FmtCode   = PCM
               | IEEEFloat
               | ALAW
               | MULAW
               | EXTENSIBLE deriving (Eq)


instance Show FmtCode where
  show PCM        = "pcm"
  show IEEEFloat  = "IEEE float"
  show ALAW       = "8-bit ITU-T G.711 A-law"
  show MULAW      = "8-bit ITU-T G.711 mu-law"
  show EXTENSIBLE = "determined by subformat"


instance Show RIFF where
  show (RIFF riffdata) =
    "RIFF " ++ show riffdata


instance Show RIFFData where
  show (WaveRIFF wavechunks) = "WAV " ++ show wavechunks
  show (OtherRIFF formtype) = "Other " ++ show formtype


instance Show WaveChunk where
  show (FmtChunk fmtinfo) = " fmt (" ++ show fmtinfo ++ ")"
  show (DataChunk dat) = " data (length=" ++ show (B.length dat) ++ " bytes)"
  show (OtherChunk fourcc dat) = " " ++ C.unpack fourcc ++ " (length=" ++ show (B.length dat) ++ " bytes)"


instance Show FmtInfo where
  show fi = "format=" ++ show (wFormatTag fi) ++
            " # channels=" ++ show (nChannels fi) ++
            " samples per sec=" ++ show (nSamplesPerSec fi) ++
            " avg bytes per sec=" ++ show (nAvgBytesPerSec fi) ++
            " data block size=" ++ show (nBlockAlign fi) ++ " bytes" ++
            " bits per sample=" ++ show (wBitsPerSample fi)


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
