module Main where

main :: IO()
main = do print 1
          arstoien
          print 2

arstoien, b, c :: IO ()
arstoien = do print 10
              b
              print 20

b = do print 100
       c
       print 200

c = do print 1000
       print $ crashSelf 3
       print 2000

crashSelf :: Int -> Int
crashSelf 0 = 1 `div` 0
crashSelf x = x + x + crashSelf (x - 1)

-- Observation, the stack only grows when you append from right
--
-- > (crashself ..) + x + x
--
-- But not when from left
--
-- > x + x + (crashself ..)
