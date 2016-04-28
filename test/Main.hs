import System.Exit (exitSuccess, exitFailure)

import WAV.Parser.Test


tests :: [Bool]
tests = [ testfourccParser
        , testLengthParser 
        , testFmtParser 
        , testDataParser ]


runTests tests = 
    if and tests
        then exitSuccess
        else do
            print tests
            exitFailure

main :: IO ()

main = do
    runTests tests
    return () 

