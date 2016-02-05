import Debug.Trace
import Data.Char
import Data.List
import Control.Exception
import Control.Monad
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

    f <- allPeaks (timeDUT i j)  []
    print f


timeDUT' sin sout test = do
    hPutStrLn sin test
    start <- getTime Monotonic
    _ <- hGetLine sout
    f <- hGetLine sout
    if f == "Welcome." 
        then 
            return Nothing              -- this means we can no longer call timeDUT' ever again as the process is dead.
        else
            do 
                end <- getTime Monotonic
                let diff = timeSpecAsNanoSecs $ diffTimeSpec start end
                return $ Just diff

makeGuess known full unknown =
    map intToDigit $ known ++ [unknown] ++ replicate (full - (length known) - 1) 0

timeDUT sin sout known candidate =
    timeDUT' sin sout $ makeGuess known 5 candidate


     -- here, we use use Right [Int] to represent latency results and Left Int to
     -- represent the process under test quitting
acc test prev digit =
    case prev of
        Left i  -> return $ Left i
        Right i -> test digit >>= \y -> case y of
            Nothing ->  return $ Left digit
            Just j  ->  return $ Right $ i ++ [j]


genTimings test known =
    foldM (acc $ test known) (Right []) [0..9]

findPeak i =
    elemIndex (maximum i) i

allPeaks test current =
    genTimings test current >>= \x -> case x of
        Left i  -> return (current ++ [i])
        Right i -> allPeaks test $ current ++ [fromJust . findPeak $ i]
