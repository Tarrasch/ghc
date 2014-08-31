import GHC.ExecutionStack
import Data.List (isInfixOf)

-- In this test we check loading and unloading of dwarf

main :: IO ()
main = do
    stack <- currentExecutionStack
    testStartsUnloaded stack
    testInDwarf stack
    testUnloading

testStartsUnloaded stack = do
    frames <- getStackFramesNoSync stack
    check $ "Data not found" `isInfixOf` show frames

testInDwarf stack = do
    frames <- getStackFrames stack
    check $ not $ "Data not found" `isInfixOf` show frames

testUnloading = do
    dwarfTryUnload >>= check  -- Unload successful
    dwarfTryUnload >>= (check . not)  -- Already unloaded

check :: Bool -> IO ()
check True  = putStrLn "OK"
check False = putStrLn "Not OK"
