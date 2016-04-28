import WAV
import WAV.Parser
import System.Environment (getArgs)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C


showFmtCode :: FmtCode -> String
showFmtCode PCM        = "pcm"
showFmtCode IEEEFloat  = "IEEE float"
showFmtCode ALAW       = "8-bit ITU-T G.711 A-law"
showFmtCode MULAW      = "8-bit ITU-T G.711 mu-law"
showFmtCode EXTENSIBLE = "determined by subformat"


showRIFF :: RIFF -> String
showRIFF (RIFF riffdata) = "valid RIFF file\n" ++ showRIFFData riffdata


showRIFFData :: RIFFData -> String
showRIFFData (WaveRIFF wavechunks) = "valid WAVE file\n" ++
  concatMap showWaveChunk wavechunks
showRIFFData (OtherRIFF formtype) = "not a WAVE file\n"


showWaveChunk :: WaveChunk -> String
showWaveChunk (FmtChunk fmtinfo) = showFmtInfo fmtinfo
showWaveChunk (DataChunk dat) = "data chunk (length=" ++ show (B.length dat) ++ " bytes)\n"
showWaveChunk (OtherChunk fourcc dat) = C.unpack fourcc ++ " chunk (length=" ++ show (B.length dat) ++ " bytes)\n"


showFmtInfo :: FmtInfo -> String
showFmtInfo fi =
  show (wFormatTag fi) ++ " format\n" ++
  show (nChannels fi) ++ " channels\n" ++
  show (nSamplesPerSec fi) ++ " samples per second\n" ++
  show (nAvgBytesPerSec fi) ++ " avg bytes per second\n" ++
  show (nBlockAlign fi) ++ " bytes per data block\n" ++
  show (wBitsPerSample fi) ++ " bits per sample\n"


main :: IO ()
main = do
  args <- getArgs
  eitherRiff <- parseFromFile $ head args
  case eitherRiff of
    Left err -> print err
    Right riff -> putStrLn $ showRIFF riff
