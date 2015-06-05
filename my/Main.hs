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
       print $ crashSelf (Nothing :: Maybe Int)
       print 2000

crashSelf :: Maybe a -> a
crashSelf (Just a) = a

-- Observation, the stack only grows when you append from right
--
-- > (crashself ..) + x + x
--
-- But not when from left
--
-- > x + x + (crashself ..)


