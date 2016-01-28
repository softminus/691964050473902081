import Debug.Trace
import Data.Char
import Control.Exception
import Control.Concurrent
import System.Process 
import System.IO
import Data.Maybe
import System.Clock

main = do
    (Just i, Just j, _, h) <- createProcess (proc "./pass" []){ std_in=CreatePipe, std_out=CreatePipe} 
    hSetBuffering i NoBuffering
    hSetBuffering j NoBuffering

    _  <- hGetLine j
    timings <-genTimings (timeRun i j) []
    print timings 


timeRun sin sout test = do
    hPutStrLn sin test
    start <- getTime Monotonic
    _ <- hGetLine sout
    _ <- hGetLine sout
    end <- getTime Monotonic
    let diff = timeSpecAsNanoSecs $ diffTimeSpec start end
    return diff



makeGuess known full unknown  =
    map intToDigit $ known ++ [unknown] ++ replicate (full - (length known) - 1) 0


genTimings test sofar =
    mapM (test . makeGuess sofar 5) $ [0..9]




