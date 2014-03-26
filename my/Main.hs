module Main where

printInt x = print (x :: Int)

main :: IO()
main = do printInt 1
          arstoien
          printInt 2

arstoien, b, c :: IO ()
arstoien = do printInt 10
              b
              printInt 20

b = do printInt 100
       c
       printInt 200

c = do printInt 1000
       printInt $ crashSelf 2
       printInt 2000

crashSelf :: Int -> Int
crashSelf 0 = 1 `div` 0
crashSelf x = crashSelf (x - 1)
