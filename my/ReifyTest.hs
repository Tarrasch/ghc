{-# LANGUAGE MagicHash, BangPatterns, ScopedTypeVariables #-}
module Main where

--import GHC.IO (reifyStack, MyArray(..))
import GHC.ExecutionStack (reifyStack, dumpStack, stackSize, ExecutionStack(..))
import GHC.Prim
import GHC.Exts
import Unsafe.Coerce
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.Primitive.ByteArray
import Data.Primitive.Types
import System.Mem
import Data.Char
import Data.Int
import Numeric

import Control.Monad (forM_)

------
--
-- First the "library"

data LocationInfo = LocationInfo { 
           functionName :: String,
           fileName :: String,
           startLine :: Int,
           startCol :: Int,
           endLine :: Int,
           endRow :: Int
           }
           deriving(Show, Eq)

instance Show ExecutionStack where 
    show es@(ExecutionStack ba#) =
         "Addr (first): " ++ show (byteArrayContents (ByteArray ba#)) ++ "\n" ++
         "Length: " ++ show (stackSize es) ++ "\n" ++
         unlines (mapByteArray (("  " ++ ) . show :: Addr -> String) (ByteArray ba#)) ++
         "Addr (again): " ++ show (byteArrayContents (ByteArray ba#)) ++ "\n\n\n\n\n"
         
instance Show Addr where
    show = ("0x" ++ ) . flip showHex "" . (unsafeCoerce :: Addr -> Int)

mapByteArray :: forall a b. Prim a => (a -> b) -> ByteArray -> [b]
mapByteArray f ba = map (f . get) [0..n - 1]
    where n     = sizeofByteArray ba `div` (I# (sizeOf# (undefined :: a)))
          get i = indexByteArray ba i


-----  Main below vvvvvvvvv
--
--
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
       d
       print 2000

d = do print 1001
       e
       print 2001

e = do print 1002
       putStrLn "\n"
       ba <- reifyStack
       print ba
       putStrLn "\n"
       reifyStack >>= print
       putStrLn "\n"
       reifyStack >>= print
       putStrLn "\n"
       reifyStack >>= print
       putStrLn "\n"
       -- reifyStack >>= print
       -- putStrLn "\n"
       -- print $ 1 `div` 0
       print 2002
