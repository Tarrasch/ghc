{-# LANGUAGE UnboxedTuples, MagicHash, RecordWildCards, ScopedTypeVariables #-}
module Main where

import GHC.IO
import GHC.ExecutionStack
import Unsafe.Coerce
import GHC.Prim

main :: IO()
main = do
    dwarfInit
    stack <- reifyStack 
    (_, du) <- dwarfLookupIp2 (stackIndex stack 0)
    print (unsafeCoerce du :: Int)

dwarfLookupIp2 :: Addr#
               -> IO (Int, DwarfUnit) -- ^ (maxNumInfos, dwarfUnit)
dwarfLookupIp2 addr# =  IO $ \s -> 
    case newByteArray# 1000# s of 
      (# new_s, mba# #) -> do
        case (dwarfLookupIp addr# mba#) of
          IO f -> f new_s
