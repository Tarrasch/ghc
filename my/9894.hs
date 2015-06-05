module Main where

-- https://ghc.haskell.org/trac/ghc/ticket/9894

import System.IO

f name = print $ "Hello, " ++ name
{-# NOINLINE f #-}

g name = f name
{-# NOINLINE g #-}

main = do
    name <- getLine
    g name
