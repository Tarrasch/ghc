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
  , currentExecutionStackLimit
  -- * Intermediate interface (May change)
  , ExecutionStack ()
  , getStackFrames
  , StackFrame(..)
  -- * Complicated interface (May change)
  -- ** Managed loading/unloading
  , inDwarf
  , dwarfIncRef
  , dwarfDecRef
  , dwarfTryUnload
  , dwarfIsLoaded
  -- ** Forceful loading/unloading
  -- , dwarfForceLoad
  -- , dwarfForceUnload
  -- ** Looking inside `ExecutionStack` value
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
import GHC.Prim (reifyStack##, countStackSize##)
import Foreign.C.String (peekCString, CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal (alloca)
import Text.Printf (printf)
import Control.Exception.Base (bracket_)

#include "Rts.h"
#include "rts/Elf.h"

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
  -- Looking at Dwarf.h, this is one DwarfUnit and many DebugInfos.

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
data DwarfUnit
data DwarfProc
data Instruction

peekDwarfUnitName :: Ptr DwarfUnit -> IO CString
peekDwarfUnitName ptr = #{peek struct DwarfUnit_, name } ptr

peekDwarfProcName :: Ptr DwarfProc -> IO CString
peekDwarfProcName ptr = #{peek struct DwarfProc_, name } ptr

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
currentExecutionStack = countStackSize >>= currentExecutionStackLimit

countStackSize :: IO (Int)
countStackSize =
    IO (\s -> let (## new_s, stackSize## ##) = countStackSize## s
                  stackSize = I## stackSize##
              in (## new_s, stackSize ##) )


currentExecutionStackLimit :: Int -> IO (ExecutionStack)
currentExecutionStackLimit (I## i##) =
    IO (\s -> let (## new_s, byteArray## ##) = reifyStack## i## s
                  ba = ExecutionStack byteArray##
              in (## new_s, ba ##) )

-- | Tell the dwarf module that you want to use dwarf. Synchronized.
foreign import ccall "Elf.h dwarf_inc_ref" dwarfIncRef :: IO ()

-- | Tell the dwarf module that you are done using dwarf. Synchronized.
foreign import ccall "Elf.h dwarf_dec_ref" dwarfDecRef :: IO ()

-- | Ask the dwarf module if it can unload and free up memory. It will not be
-- able to if the module is in use or if data is not loaded. Synchronized.
--
-- Returns True if dwarf data was unloaded.
foreign import ccall "Elf.h dwarf_try_unload" dwarfTryUnload :: IO Bool

foreign import ccall "Elf.h dwarf_is_loaded" dwarfIsLoaded :: IO Bool

-- | Lookup an instruction pointer
--
-- Dwarf module must be loaded to use this!
foreign import ccall "Elf.h dwarf_lookup_ip"
    dwarfLookupIp ::
       Ptr Instruction -- ^ Code address you want information about
    -> Ptr (Ptr DwarfProc) -- ^ Out: DwarfProc Pointer Pointer
    -> Ptr (Ptr DwarfUnit) -- ^ Out: DwarfUnit Pointer Pointer
    -> IO CInt -- ^ TODO(arash): what to say?

inDwarf :: IO a -> IO a
inDwarf = bracket_ dwarfIncRef dwarfDecRef

getStackFrame ::
       Ptr Instruction -- ^ Instruction Pointer
    -> IO StackFrame -- ^ Result
getStackFrame ip =
    inDwarf $ getStackFrameNoSync ip

getStackFrameNoSync ::
       Ptr Instruction -- ^ Instruction Pointer
    -> IO StackFrame -- ^ Result
getStackFrameNoSync ip = do
    alloca $ \ppDwarfProc -> do
      poke ppDwarfProc nullPtr
      alloca $ \ppDwarfUnit -> do
          dwarfLookupIp ip
                        ppDwarfProc
                        ppDwarfUnit
          pDwarfProc <- peek ppDwarfProc
          pDwarfUnit <- peek ppDwarfUnit
          unitName <- stringPeekWith peekDwarfUnitName pDwarfUnit
          procedureName <- stringPeekWith peekDwarfProcName pDwarfProc
          return StackFrame{..}

-- Note: if you grepped your way to the string "<Data not found>,
-- you probably forgot to compile that module with the `-g` flag to ghc.
stringPeekWith :: (Ptr a -> IO CString) -> Ptr a -> IO String
stringPeekWith _peeker ptr | ptr == nullPtr = return "<Data not found>"
stringPeekWith peeker ptr  | otherwise      = peeker ptr >>= peekCString

getStackFrames :: ExecutionStack -> IO [StackFrame]
getStackFrames stack = inDwarf $ getStackFramesNoSync stack

getStackFramesNoSync :: ExecutionStack -> IO [StackFrame]
getStackFramesNoSync = mapM getStackFrameNoSync . stackIndices
