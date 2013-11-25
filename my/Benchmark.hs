import Control.Exception (catch, SomeException (SomeException), evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl')
import System.CPUTime (getCPUTime)

main :: IO()
main = do
    evaluate (stackBuilder 1) -- For fairness with CAFs
    putStrLn $ "Takes " ++ show fast ++ "ms with small stack"
    putStrLn $ "Takes " ++ show slow ++ "ms with large stack"
  where
    fast = stackBuilder 0
    slow = stackBuilder 1000

getMilliSeconds :: IO Integer
getMilliSeconds = fmap (`div` 1000000000) getCPUTime

timeIt :: Integer -> IO Integer
timeIt x = do t0 <- getMilliSeconds
              evaluate x
              t <- getMilliSeconds
              return (t - t0)

-- Returns the time it takes to evaluate stackDependent
stackBuilder :: Integer -> Integer
stackBuilder x | x < 1 = unsafePerformIO (timeIt (stackDependent x))
stackBuilder x         = x - x + stackBuilder (x-1) -- Add frames on the stack

-- It has to take an argument to not become a thunk
stackDependent :: Integer -> Integer
stackDependent zero = foldl' (+) 0 (map stupidFunction [1..(zero + 1000000)])

stupidFunction :: Integer -> Integer
stupidFunction x = unsafePerformIO (action `catch` handler)
  where
    action = evaluate (5 `div` 0)
    handler (SomeException _) = return x
