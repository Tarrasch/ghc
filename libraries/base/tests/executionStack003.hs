import GHC.ExecutionStack
import Debug.Trace
import GHC.IO (unsafePerformIO)
import Data.List (isPrefixOf, replicate)

-- Are ticks across specializations maintained?
--
-- I also did eyeball the output of
--
-- $ ./inplace/bin/ghc-stage2 -ddump-simpl -fspecialise -O \
--     libraries/base/tests/executionStack003.hs | less
--
-- There I make sure that the general version gets replaced with one
-- specialized for Int.
--
-- Also, it might be interesting to pass the flags '-g -dppr-ticks' if you
-- want to do manual inspection

myTypeclassFunction :: (Num a) => [a] -> a
{-# SPECIALISE myTypeclassFunction :: [Int] -> Int #-}
myTypeclassFunction []     = fromIntegral $ unsafePerformIO $ findOccurencesOf "myTypeclassFunction"
myTypeclassFunction (x:xs) = myTypeclassFunction xs + x

findOccurencesOf :: String -> IO Int
findOccurencesOf fname = do
    stack <- currentExecutionStack
    stackFrames <- getStackFrames stack
    let infos = concatMap locationInfos stackFrames
        functionNames = map functionName infos
    return $ length . filter (isPrefixOf fname) $ functionNames

formatter x | x > 20    = "Wut?"
            | x > 2     = "Dwarf data remains when specializing! :)"
            | otherwise = "Debug data gets lost when specializing"

main = putStrLn
      . formatter
      . (myTypeclassFunction :: [Int] -> Int)
      $ replicate 5 (0 :: Int)
     -- XXX: We only recurse a few times in myTypeclassFunction,
     -- otherwise we'll get marked update frames, and we won't be able to
     -- resolve the function names on them.

-- Historical note: Back when I worked on a ghc branch from late 2013, before
-- 7822166, I also had a test where I tested that the sum function gets put on
-- the stack. After rebasing I suddenly saw that my test case broke because the
-- sum function is now defined as a fold and is inlined. :)
