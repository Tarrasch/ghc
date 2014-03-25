module Main where

import Control.Exception (catch, SomeException (SomeException), evaluate)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl')

main :: IO()
main = do
    putStrLn $ "Takes " ++ show fast ++ " seconds with small stack"
    putStrLn $ "Takes " ++ show slow ++ " seconds with large stack"
  where
    fast = (stackBuilder 100)
    slow = (stackBuilder 10000)

-- We start here, first we try to put a lot of stuff on the execution stack
step1 :: Int
step1 = stackBuilder 100000

-- This function is extremely unpure in reality, it will return the time it took to evaluate
-- step2
stackBuilder :: Int -> Double
stackBuilder 0 = timeIt (step2 )
stackBuilder x = x + (stackBuilder (x-1)) -- Add frames on the stack

fastSum :: [Int] -> Int
fastSum = foldl' (+) 0

-- Next we are here with a big stack behind us probably, we will not perform a
-- pure calculation that relies on exception handling for control flow
step2 :: Int -> Int
step2 upto = fastSum (map stupidFunction [1..upto])

stupidFunction :: Int -> Int
stupidFunction x | odd x     = x
                 | otherwise = unsafePerformIO (action `catch` handler)
  where
    action  = evaluate ((5 :: Int) `div` 0)
    handler (SomeException _) = return (-x)
