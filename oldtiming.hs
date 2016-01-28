import Control.Exception
import Control.Concurrent
import System.Process 
import System.IO
import Data.Maybe
import System.Clock

main =  do 
    (Just i, Just j, _, h) <- createProcess (proc "./pass" []){ std_in=CreatePipe, std_out=CreatePipe}
    hSetBuffering i NoBuffering
    hSetBuffering j NoBuffering
    f <- hGetLine j
    putStrLn $ "first stripped is: " ++ f

    timeRun i j 10000 
    putStrLn "eg"
    timeRun i j 20000 
    timeRun i j 24000 


timeRun sin sout test = do
    hPutStrLn sin $ show test
    start <- getTime Monotonic
    f <- hGetLine sout                                   -- first thing, please kill l8r and strip off first line earlier
    putStrLn $ "second tripped is: " ++ f
    f <- hGetLine sout                                   -- first thing, please kill l8r and strip off first line earlier
    putStrLn $ "third stripped is: " ++ f
    end <- getTime Monotonic
    let diff = diffTimeSpec start end
    putStrLn (show diff)
    return diff



