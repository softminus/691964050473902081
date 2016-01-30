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
    print "egg"
   -- allPeaks (timeRun i j) []
  --  f <-genTimings (timeRun i j) [2,4,6,5]
   --  print f 
    

--    print peaks


timeDUT' sin sout test = do
    hPutStrLn sin test
    start <- getTime Monotonic
    _ <- hGetLine sout
    f <- hGetLine sout
    if f == "Welcome." 
        then 
            return Nothing              -- this means we can no longer call timeRun, the process is dead.
        else
            do 
                end <- getTime Monotonic
                let diff = timeSpecAsNanoSecs $ diffTimeSpec start end
                return $ Just diff

makeGuess known full unknown =
    map intToDigit $ known ++ [unknown] ++ replicate (full - (length known) - 1) 0

timeDUT sin sout known candidate =
    (timeDUT' sin sout $ makeGuess known 5 candidate) >>= \ x -> case x of
        Nothing -> return $ Left candidate
        Just t  -> return $ Right t



--genTimings test known =
 --   genTimings' (test . makeGuess known 5) known 

--genTimings' test known =
--     foldM (acc test) (Right []) [0..9]
--     mapM (test . makeGuess known 5) $ [0..9]

     -- here, we use use Right [Int] to represent latency results and Left Int to
     -- represent the process under test quitting
--acc test res digit =
--    case res of
--        Left i  -> return (Left i)        -- just preserve the bloody thing
 --       Right i -> case test digit of
   --         Nothing -> return (Left digit)
     --       Just j  -> return $ (Right $ i ++ [j])



findPeak timings =
    elemIndex (maximum timings) timings





-- allPeaks test current =
--    do
--        next <- fromJust . findPeak <$> genTimings test current
 --       print next
   --     allPeaks test (current++[next]) 



