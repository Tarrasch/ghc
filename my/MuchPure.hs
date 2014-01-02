module Main where

-- again we want to test stack traces. But lets do a lot of pure
-- computations and not IO

collatz :: Int -> [Int]
collatz 1 = [1]
collatz 13 = error "I'm with stupid"
collatz n = n : if (odd n)
                  then (collatz (3 * n + 1))
                  else (collatz (n `div` 2))

collatzMain = mapM print (reverse (collatz 7))

fib :: Int -> Int
fib 0 = undefined
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

main = collatzMain
