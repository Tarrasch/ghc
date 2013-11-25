module TracePrint where

import GHC.IO (unsafePerformIO, tracePrint)

main :: IO()
main = do print 1
          a
          print 2

a, b, c :: IO ()
a = do print 10
       b
       print 20

b = do print 100
       c
       print 200

c = do print 1000
       print $ double2 4
       print 2000

crashSelf :: Int -> Int
crashSelf 0 = 1 `div` 0
crashSelf x = x + x + crashSelf (x `div` 2)

double, double2, double3 :: Int -> Int
double x = tP $ x + x
double2 x = x + (tP x)
double3 x = x + ((tP x) + (tP x))

tP :: a -> a
tP = unsafePerformIO . tracePrint
