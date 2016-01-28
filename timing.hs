import Data.Char
import Control.Exception
import Control.Concurrent
import System.Process 
import System.IO
import Data.Maybe
import System.Clock

main =  createProcess (proc "./pass" []){ std_in=CreatePipe, std_out=CreatePipe} >>= \(i, j, k, h) -> (timeRun (fromJust i) (fromJust j) 10000)


timeRun sin sout test = do
    hGetLine sout                                   -- first thing, please kill l8r and strip off first line earlier
    hPutStrLn sin $ show test
    start <- getTime Monotonic
    hGetLine sout
    hGetLine sout
    end <- getTime Monotonic
    fuck <- hGetLine sout                           -- remove me i'm just the runt end of "password"K
    
    putStrLn $ "fuck you " ++ fuck
    let diff = diffTimeSpec start end
    putStrLn (show diff)
    return diff




makeGuess known full unknown  =
    map intToDigit $ known ++ [unknown] ++ replicate (full - (length known) - 1) 0

findIt sofar =
    map (makeGuess sofar 5) [0..9] 

