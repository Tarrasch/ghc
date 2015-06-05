module Main where

import GHC.ExecutionStack
import System.IO.Unsafe (unsafePerformIO)
import Data.List(isInfixOf)

main :: IO()
main = do print 1
          print (judgeIfStackTracesWorks (dontInlineMe (unsafePerformIO getStackString)))
          print 2

getStackString :: IO String
getStackString = do
    stack <- currentExecutionStack
    frames <- getStackFrames stack
    return $ unlines $ map show frames

{-# NOINLINE dontInlineMe #-}
dontInlineMe :: String -> String
dontInlineMe x = case x of
                   "hello" -> "world"
                   x       -> x

judgeIfStackTracesWorks :: String -> Bool
judgeIfStackTracesWorks = ("dontInlineMe" `isInfixOf`)

-- crashSelf :: Maybe a -> Int
-- crashSelf = \ma -> case ma of 
--                 Nothing -> seq (unsafePerformIO printCurrentExecutionStack) 5
--                 Just a  -> seq (unsafePerformIO printCurrentExecutionStack) 6

-- crashSelf :: Maybe a -> a
-- crashSelf ~(Just a) = seq (unsafePerformIO printCurrentExecutionStack) a
