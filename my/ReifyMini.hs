{-# LANGUAGE MagicHash #-}
module Main where

import GHC.IO (reifyStack, MyArray(..))
import GHC.Prim
import GHC.Exts
import Unsafe.Coerce
import Data.Primitive.ByteArray
import Data.Primitive.Types
import System.Mem
import Data.Char
import Data.Int
import Control.Monad (forM_)

reifyStack' :: IO (ByteArray)
reifyStack' = do
    myba <- reifyStack
    return $ unsafeCoerce myba

mapByteArray :: Prim a => (a -> b) -> ByteArray -> [b]
mapByteArray f ba = map (f . get) [0..n - 1]
    where n     = sizeofByteArray ba
          get i = indexByteArray ba i

instance Show Addr where
    show = show . (unsafeCoerce :: Addr -> Int)

printStackTrace :: ByteArray -> IO()
printStackTrace ba = do
    forM_ [0..n-1] (\i -> putStrLn $ "   " ++ show (get i))
  where n     = sizeofByteArray ba
        get :: Int -> Addr
        get i = indexByteArray ba i


-----  Main below vvvvvvvvv
--
--
main :: IO()
main = do print 1
          a
          print 2

a, b :: IO()
a = do print 10
       b
       print 20

b = do print 100
       putStrLn $ "Running time 1"
       reifyStack' >>= printStackTrace
       putStrLn $ "Running time 2"
       reifyStack' >>= printStackTrace
       putStrLn $ "Running time 3"
       reifyStack' >>= printStackTrace
       print 200
