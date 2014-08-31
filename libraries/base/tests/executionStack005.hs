import GHC.ExecutionStack
import Data.List (isInfixOf)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent (forkIO)

-- In this test we check that it is thread-safe, which means that a thread
-- can't unload the dwarf data while another thread is in an `inDwarf` block.

dwarfIsLoaded :: IO Bool
dwarfIsLoaded = fmap (("Data not found" `isInfixOf`) . show) currentExecutionStack

main :: IO ()
main = do
    var1 <- newEmptyMVar
    var2 <- newEmptyMVar
    dwarfIsLoaded >>= (check . not)       -- 1
    dwarfTryUnload >>= (check . not)
    thread <- forkIO $ do
        takeMVar var1
        -- When doing dwarfIsLoaded the check here, we also verify that other
        -- threads can access the dwarf info. But I'm not sure how to test that
        -- a different unix thread can access the dwarf info as well.
        dwarfIsLoaded >>= check           -- 3
        dwarfTryUnload >>= (check . not)
        dwarfIsLoaded >>= (check . not)
        putMVar var2 ()
    inDwarf $ do
        dwarfIsLoaded >>= check           -- 2
        dwarfTryUnload >>= (check . not)
        putMVar var1 ()
        takeMVar var2
    inDwarf $ return ()
    dwarfIsLoaded >>= check               -- 4
    dwarfTryUnload >>= check


check :: Bool -> IO ()
check True  = putStrLn "OK"
check False = putStrLn "Not OK"
