{-# LANGUAGE MagicHash, BangPatterns #-}
module Main where

-- import GHC.IO (reifyStack, ExecutionStack, dumpStack)
--import GHC.IO (reifyStack, MyArray(..))
import GHC.ExecutionStack (reifyStack, dumpStack)
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

import Control.Monad (forM_)

-----  Main below vvvvvvvvv
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
       reifyStack >>= dumpStack
       reifyStack >>= dumpStack
       reifyStack >>= dumpStack
       print 2002

