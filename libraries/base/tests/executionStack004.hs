import GHC.ExecutionStack
import Data.List (isInfixOf)

-- In this test we check loading and unloading of dwarf

main :: IO ()
main = do
    dwarfTryUnload >>= (check . not) -- Starts unloaded
    _frames <- currentExecutionStack >>= getStackFrames
    dwarfTryUnload >>= check  -- Unload successful
    dwarfTryUnload >>= (check . not)  -- Already unloaded
    dwarfTryUnload >>= (check . not)  -- Already unloaded

check :: Bool -> IO ()
check True  = putStrLn "OK"
check False = putStrLn "Not OK"
