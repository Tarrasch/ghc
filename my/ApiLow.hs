{-# LANGUAGE MagicHash, UnboxedTuples, RecordWildCards #-}
module Main where

import GHC.IO (reifyStack
             , dumpStack
             , IO(..)
             , ExecutionStack(..)
             , unsafePerformIO)
import GHC.Prim
import GHC.Exts
import Unsafe.Coerce
import Data.Primitive.ByteArray
import Data.Primitive.Types
import System.Mem
import Data.Char
import Data.Int
import Data.Word
import Control.Monad (forM_)
import Foreign.C.String

import Data.Primitive.MachDeps



dwarfInit :: IO ()
dwarfInit = do
    putStrLn "Initializing dwarf"
    IO $ \s -> let s' = dwarfInit# s
               in (# s', () #)

dwarfFree :: IO ()
dwarfFree = do
    putStrLn "Deinitializing dwarf"
    IO $ \s -> let s' = dwarfFree# s
               in (# s', () #)

unES :: ExecutionStack -> ByteArray
unES (ExecutionStack ba) = ByteArray ba

stackSize :: ExecutionStack -> Int
stackSize = sizeofByteArray . unES

data LocationInfo = LocationInfo { 
           startLine :: Word16,
           startCol :: Word16,
           endLine :: Word16,
           endCol :: Word16,
           fileName :: String,
           functionName :: String
           }
           deriving(Show, Eq)

-- A location info format that matches the C struct `DebugInfo_`, from
-- dwarf.h
data RawLocationInfo = RawLocationInfo { 
           raw_startLine :: Word16
         , raw_startCol :: Word16
         , raw_endLine :: Word16
         , raw_endCol :: Word16
         , raw_fileName :: Addr -- s/Addr/CString
         , raw_functionName :: Addr -- s/Addr/CString
           -- depth :: Word16 -- TODO
           }
           deriving(Show, Eq)

unI# :: Int -> Int#
unI# (I# n#) = n#

instance Prim RawLocationInfo where
  sizeOf# _ = unI# (4*sIZEOF_WORD16 + sIZEOF_PTR + sIZEOF_PTR + sIZEOF_WORD)
        -- TODO: Can I use c-preprocessor or something here? Probably not
        -- but ....
  alignment# _ = undefined
  indexByteArray# = indexRawLocationInfoArray# 
  readByteArray#  arr# i# s# =  undefined
  writeByteArray# arr# i# = undefined
                                                              
  indexOffAddr# addr# i# = undefined
  readOffAddr#  addr# i# s# = undefined
  writeOffAddr# addr# i# = undefined

  -- alignment# _ = unI# align                                   
  -- indexByteArray# arr# i# = ctr (idx_arr arr# i#)             
  -- readByteArray#  arr# i# s# = case rd_arr arr# i# s# of      
  --                       { (# s1#, x# #) -> (# s1#, ctr x# #) }
  -- writeByteArray# arr# i# (ctr x#) s# = wr_arr arr# i# x# s#  
                                                              
  -- indexOffAddr# addr# i# = ctr (idx_addr addr# i#)            
  -- readOffAddr#  addr# i# s# = case rd_addr addr# i# s# of     
  --                       { (# s1#, x# #) -> (# s1#, ctr x# #) }
  -- writeOffAddr# addr# i# (ctr x#) s# = wr_addr addr# i# x# s# 



-- type Ip = Ptr Instruction

stackIndex :: ExecutionStack -> Int -> Addr
stackIndex es i = indexByteArray (unES es) i

dwarfLookupIp :: Addr -> MutableByteArray RealWorld -> IO (Int, Addr)
dwarfLookupIp (Addr addr#) (MutableByteArray mba#) = 
    let (# numWritten#, dwarfUnit# #) = dwarfLookupIp# addr# mba#
    in (I# numWritten#, Addr dwarfUnit#)

readFirst :: ByteArray -> LocationInfo
readFirst ba = LocationInfo {..}
  where n     = sizeofByteArray ba
        get i = indexByteArray ba i
        [startLine, startCol, endLine, endCol] = 
          take 4 $ mapByteArray (id :: Word16 -> Word16) ba
        cStrings =
          take 2 $ drop 1 $ mapByteArray (unsafeCoerce :: Addr -> CString) ba
          -- INCORRECT: We assume (CString/(Ptr CChar)) and Int is same size
          -- Ideally I wanted (id :: CString -> CString), but CString has
          -- no prim instance. I know they both have the same
          -- implementation. So it's safe at the moment, hence a TODO
        [fileName, functionName] =
          unsafePerformIO $ mapM peekCString cStrings 
     -- unsafePerformIO is safe since they are pointers to static memory

indexRawLocationInfoArray# :: ByteArray# -> Int# -> RawLocationInfo
indexRawLocationInfoArray# arr# i# = RawLocationInfo {..}
  where
    offset = i# *# sizeOf# (undefined :: RawLocationInfo)
    baseAddr# = byteArrayContents# arr# `plusAddr#` offset  -- TODO: Not safe unless pinned!
    relIdx :: Prim a => Int# -> a
    relIdx = indexOffAddr# baseAddr# 
    raw_startLine = relIdx (unI# 0)
    raw_startCol = relIdx (unI# 1)
    raw_endLine = relIdx (unI# 2)
    raw_endCol = relIdx (unI# 3)
    raw_fileName = relIdx (unI# 1) -- TODO: We assume a pointer is 64-bit, same as four 16-bit
    raw_functionName = relIdx (unI# 2)



convertLocationInfo :: RawLocationInfo -> LocationInfo
convertLocationInfo rli = undefined -- LocationInfo {..}
        where a = a

mapByteArray :: Prim a => (a -> b) -> ByteArray -> [b]
mapByteArray f ba = map (f . get) [0..n - 1]
    where n     = sizeofByteArray ba
          get i = indexByteArray ba i


inDwarf :: IO () -> IO ()
inDwarf action =  do
    dwarfInit
    action
    dwarfFree

-----  Main below vvvvvvvvv
--
--
main :: IO()
main = do
    putStrLn "Lol"
    mba <- newByteArray 100
    inDwarf $ do
      stack <- reifyStack
      -- reifyStack >>= dumpStack
      putStrLn $ "Size of stack: " ++ show (stackSize stack)
      putStrLn "About to lookup IP"
      print $ fst $ dwarfLookupIp (stackIndex stack 1) mba
      ba <- unsafeFreezeByteArray mba
      putStrLn "About to do readFirst!"
      print $ readFirst ba
      putStrLn "About to do readFirst2!"
      print $ (indexByteArray ba 0 :: RawLocationInfo)
      putStrLn "lol"

instance Show Addr where
    -- show = show . (unsafeCoerce :: Addr -> Int)
    -- show addr = unsafePerformIO (peekCString (unsafeCoerce addr))
    show = unsafePerformIO . peekCString . unsafeCoerce 
    -- show = ("\"" ++) . (++ "\"") . unsafePerformIO . peekCString . unsafeCoerce 
