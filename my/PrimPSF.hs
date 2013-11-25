{-# LANGUAGE MagicHash #-}
module Main where

import GHC.Prim

main :: IO()
main = do print (pushStackFrame# 1)
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
       print (crashSelf 2)
       print 2000

crashSelf :: Int -> Int
crashSelf 0 = 1 `div` 0
crashSelf x = pushStackFrame# (crashSelf (x - 1))
