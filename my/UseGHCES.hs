{-# LANGUAGE UnboxedTuples, MagicHash, RecordWildCards, ScopedTypeVariables #-}
module Main where

import GHC.Base (Int(..))
import GHC.ExecutionStack
import Control.Monad
import Data.Primitive.ByteArray
import Data.Primitive.Types
import Data.Primitive.Types
import Unsafe.Coerce
import Numeric

main :: IO()
main =
    do print 1
       arstoien
       print 2

arstoien, b, c :: IO ()
arstoien = do print 10
              b
              print 20

b = do print 100
       c
       print 200

c = do print 1000
       gogoES
       print 2000

-- instance Show ExecutionStack where 
--     show es@(ExecutionStack ba#) =
--          -- "Addr (first): " ++ show (byteArrayContents (ByteArray ba#)) ++ "\n" ++
--          "Length: " ++ show (stackSize es) ++ "\n" ++
--          unlines (mapByteArray (("  " ++ ) . show :: Addr -> String) (ByteArray ba#)) ++
--          -- "Addr (again): " ++ show (byteArrayContents (ByteArray ba#)) ++ "\n\n\n\n\n" ++
--          ""
         
-- instance Show Addr where
--     show = ("0x" ++ ) . flip showHex "" . (unsafeCoerce :: Addr -> Int)

mapByteArray :: forall a b. Prim a => (a -> b) -> ByteArray -> [b]
mapByteArray f ba = map (f . get) [0..n - 1]
    where n     = sizeofByteArray ba `div` (I# (sizeOf# (undefined :: a)))
          get i = indexByteArray ba i


gogoES :: IO ()
gogoES = do
    dwarfInit -- We are doing this because the implementation isn't
              -- complete and doesn't do this automatically
    stack <- reifyStack
    print stack
    dumpStack stack
    -- dwarfInit
    let n = stackSize stack
    -- putStrLn $ "Reified " ++ show n ++ " stack frames"
    -- reifyStack
    -- print stack
    -- reifyStack
    -- print stack
    -- reifyStack
    -- print stack
    dwarfInit 
    forM_ [0..(n-1)] $ \ix -> do
      let traces = getLocationInfos stack ix 5
      -- print traces
      -- print stack
      -- dumpStack stack
      case traces of
        x:rest -> putStrLn $  showLocationInfo x -- Just print one
        [] -> putStrLn "... unknown ..."
