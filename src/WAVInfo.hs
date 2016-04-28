import WAVParser
import System.Environment (getArgs)


main :: IO ()
main = do
  args <- getArgs
  riff <- parseFromFile $ head args
  print riff
