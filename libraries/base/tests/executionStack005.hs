import GHC.ExecutionStack
import Data.List (isInfixOf)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent (forkIO)

-- In this test we check that it's thread-safe

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

