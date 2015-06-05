import GHC.ExecutionStack
import Data.List (isInfixOf)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent (forkIO)

-- In this test we check that it is thread-safe, which means that a thread
-- can't unload the dwarf data while another thread is in an `inDwarf` block.

-- Can the thread executing this access dwarf (load it before you call)
checkCanAccessDwarf :: IO Bool
checkCanAccessDwarf = do
    frames <- currentExecutionStack >>= getStackFramesNoSync
    _ <- currentExecutionStack >>= getStackFrames -- Just ensure locking works
    return $ not $ "Data not found" `isInfixOf` (show frames)

main :: IO ()
main = do
    var1 <- newEmptyMVar
    var2 <- newEmptyMVar
    putStrLn "1"
    dwarfIsLoaded >>= (check . not)
    dwarfTryUnload >>= (check . not)
    _thread <- forkIO $ do
        takeMVar var1
        -- When doing dwarfIsLoaded the check here, we also verify that other
        -- threads can access the dwarf info. But I'm not sure how to test that
        -- a different unix thread can access the dwarf info as well.
        putStrLn "3"
        dwarfIsLoaded >>= check
        checkCanAccessDwarf >>= check
        dwarfTryUnload >>= (check . not)
        dwarfIsLoaded >>= check
        putMVar var2 ()
    inDwarf $ do
        putStrLn "2"
        dwarfIsLoaded >>= check
        dwarfTryUnload >>= (check . not)
        putMVar var1 ()
        takeMVar var2
    putStrLn "4"
    inDwarf $ return ()
    dwarfIsLoaded >>= check
    dwarfTryUnload >>= check


check :: Bool -> IO ()
check True  = putStrLn "OK"
check False = putStrLn "Not OK"