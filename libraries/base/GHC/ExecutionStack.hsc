-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.ExecutionStack
-- Copyright   :  (c) The University of Glasgow 2013-2015
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- This is a module for the efficient but inaccurate stack traces. If you
-- can take a factor 2 of performance penalty, you should consider using
-- "GHC.Stack" as the stack traces will be more accurate and detailed.
--
-- @
-- myFunction :: IO ()
-- myFunction = do
--      stack <- currentExecutionStack 100
--      printExecutionStack stack
-- @
--
-- An 'ExecutionStack' is a data wrapper around 'ByteArray#'. The
-- Array is a reified stack. Each element is a code address. For most
-- frames this will be the return address for the stack frame, but
-- for an update frame the address is the entry code of the thunk (if
-- available)
--
-- /Since: 4.11.0.0/
-----------------------------------------------------------------------------

{-# LANGUAGE UnboxedTuples, MagicHash, RecordWildCards #-}
module GHC.ExecutionStack (
  -- * Simple interface
    printCurrentExecutionStack
  , currentExecutionStack
  , printExecutionStack
  -- * Intermediate interface (May change)
  , ExecutionStack ()
  , currentExecutionStackLimit
  , getStackFrames
  , StackFrame(..)
  -- * Complicated interface (May change)
  -- ** Managed loading/unloading
  , inCodemap
  , codemapIncRef
  , codemapDecRef
  , codemapTryUnload
  , codemapIsLoaded
  -- ** Looking inside the `ExecutionStack` value
  , stackSize
  , stackIndex
  , stackIndices
  -- ** Looking up instructions
  , getStackFramesNoSync
  , getStackFrame
  , getStackFrameNoSync
  ) where

import GHC.IO (IO(..))
import GHC.Base (Int(I##))
import GHC.Ptr (Ptr(Ptr))
import GHC.Prim (ByteArray##, sizeofByteArray##, indexAddrArray##)
import GHC.Prim (reifyStack##)
import Foreign.C.String (peekCString, CString)
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (alloca)
import Text.Printf (printf)
import Control.Exception.Base (bracket_)

#include "Rts.h"

data ExecutionStack = ExecutionStack
    { unExecutionStack ::  ByteArray##
    }

-- | The number of functions on your stack
stackSize :: ExecutionStack -> Int
stackSize stack =
    I## (sizeofByteArray## (unExecutionStack stack)) `div` (#const SIZEOF_HSPTR)

stackIndex :: ExecutionStack -> Int -> Ptr Instruction
stackIndex (ExecutionStack ba##) (I## i##) = Ptr (indexAddrArray## ba## i##)

stackIndices :: ExecutionStack -> [Ptr Instruction]
stackIndices stack = map (stackIndex stack) [0..(stackSize stack)-1]

data StackFrame = StackFrame
    { unitName      :: !String -- ^ From symbol table
    , procedureName :: !String -- ^ From symbol table
    } deriving Show

-- | Like 'show', without @unlines@
prepareStackFrame :: StackFrame -> [String]
prepareStackFrame su =
        [procedureName su]
    --  Note: We intend to have multiple lines per frame once we have dwarf


-- | Pretty print the execution stack to stdout
printExecutionStack :: ExecutionStack -> IO ()
printExecutionStack stack = do
    frames <- getStackFrames stack
    putStrLn $ displayFramesWithHeader frames

displayFramesWithHeader :: [StackFrame] -> String
displayFramesWithHeader frames =
    "Stack trace (Haskell):\n" ++
    concatMap (uncurry displayFrame) ([0..] `zip` frames)

-- | How one StackFrame is displayed in one stack trace
displayFrame :: Int -> StackFrame -> String
displayFrame ix frame = unlines $ zipWith ($) formatters strings
      where formatters = (printf "%4u: %s" (ix :: Int)) : repeat ("      " ++)
            strings    = prepareStackFrame frame

-- We use these three empty data declarations for type-safety
data CodemapUnit
data CodemapProc
data Instruction

peekCodemapUnitName :: Ptr CodemapUnit -> IO CString
peekCodemapUnitName ptr = #{peek struct CodemapUnit_, name } ptr

peekCodemapProcName :: Ptr CodemapProc -> IO CString
peekCodemapProcName ptr = #{peek struct CodemapProc_, name } ptr

-- | Print the current execution stack. The result printed to stdout should
-- look something like this:
--
-- @
-- Loading debug data...
-- Stack trace:
--    1: reifyStack.(...) (at libraries/base/GHC/ExecutionStack.hs:87:54-87:67)
--    2: bindIO (at libraries/base/GHC/Base.lhs:609:61-609:77)
--    3: stg_catch_frame_ret (at rts/Exception.cmm:365:1-367:2)
-- @
--
-- Even though the stack trace is somewhat cryptic, it could be as useful as a
-- tiny lantern in complete darkness.
printCurrentExecutionStack :: IO ()
printCurrentExecutionStack = currentExecutionStack >>= printExecutionStack

-- | Reify the stack. This is the only way to get an ExecutionStack value.
currentExecutionStack :: IO (ExecutionStack)
currentExecutionStack = currentExecutionStackLimit (maxBound :: Int)


currentExecutionStackLimit :: Int -> IO (ExecutionStack)
currentExecutionStackLimit (I## i##) =
    IO (\s -> let (## new_s, byteArray## ##) = reifyStack## i## s
                  ba = ExecutionStack byteArray##
              in (## new_s, ba ##) )

-- | Tell the codemap module that you want to use codemap. Synchronized.
foreign import ccall "Codemap.h codemap_inc_ref" codemapIncRef :: IO ()

-- | Tell the codemap module that you are done using codemap. Synchronized.
foreign import ccall "Codemap.h codemap_dec_ref" codemapDecRef :: IO ()

-- | Ask the codemap module if it can unload and free up memory. It will not be
-- able to if the module is in use or if data is not loaded. Synchronized.
--
-- Returns True if codemap data was unloaded.
foreign import ccall "Codemap.h codemap_try_unload" codemapTryUnload :: IO Bool

foreign import ccall "Codemap.h codemap_is_loaded" codemapIsLoaded :: IO Bool

-- | Lookup an instruction pointer
--
-- Codemap module must be loaded to use this!
foreign import ccall "Codemap.h codemap_lookup_ip"
    codemapLookupIp ::
       Ptr Instruction -- ^ Code address you want information about
    -> Ptr (Ptr CodemapProc) -- ^ Out: CodemapProc Pointer Pointer
    -> Ptr (Ptr CodemapUnit) -- ^ Out: CodemapUnit Pointer Pointer
    -> IO ()

inCodemap :: IO a -> IO a
inCodemap = bracket_ codemapIncRef codemapDecRef

getStackFrame ::
       Ptr Instruction -- ^ Instruction Pointer
    -> IO StackFrame -- ^ Result
getStackFrame ip =
    inCodemap $ getStackFrameNoSync ip

getStackFrameNoSync ::
       Ptr Instruction -- ^ Instruction Pointer
    -> IO StackFrame -- ^ Result
getStackFrameNoSync ip = do
    alloca $ \ppCodemapProc -> do
      poke ppCodemapProc nullPtr
      alloca $ \ppCodemapUnit -> do
          codemapLookupIp ip
                        ppCodemapProc
                        ppCodemapUnit
          pCodemapProc <- peek ppCodemapProc
          pCodemapUnit <- peek ppCodemapUnit
          unitName <- stringPeekWith peekCodemapUnitName pCodemapUnit
          procedureName <- stringPeekWith peekCodemapProcName pCodemapProc
          return StackFrame{..}

-- Note: if you grepped your way to the string "<Data not found>,
-- you probably forgot to compile that module with the `-g` flag to ghc.
stringPeekWith :: (Ptr a -> IO CString) -> Ptr a -> IO String
stringPeekWith _peeker ptr | ptr == nullPtr = return "<Data not found>"
stringPeekWith peeker ptr  | otherwise      = peeker ptr >>= peekCString

getStackFrames :: ExecutionStack -> IO [StackFrame]
getStackFrames stack = inCodemap $ getStackFramesNoSync stack

getStackFramesNoSync :: ExecutionStack -> IO [StackFrame]
getStackFramesNoSync = mapM getStackFrameNoSync . stackIndices
