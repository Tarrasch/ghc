{-# LANGUAGE UnboxedTuples, MagicHash, RecordWildCards, ScopedTypeVariables #-}
module Main where

import GHC.ExecutionStack

main :: IO()
main = do
    dwarfInit
    stack <- reifyStack 
    print stack
