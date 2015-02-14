{-# LANGUAGE GADTs, CPP #-}

-----------------------------------------------------------------------------
--
-- Debugging data
--
-- Association of debug data on the Cmm level, with methods to encode it in
-- event log format for later inclusion in profiling event logs.
--
-----------------------------------------------------------------------------

module Debug (

  DebugBlock(..), dblIsEntry,
  UnwindTable, UnwindExpr(..),
  cmmDebugGen,
  cmmDebugLabels,
  cmmDebugLink,
  debugToMap,
  writeDebugToEventlog

  ) where

import Binary
import BlockId         ( blockLbl )
import CLabel
import Cmm
import CmmUtils
import CoreSyn
import DynFlags
import FastString      ( nilFS, mkFastString, unpackFS )
import Module
import Outputable
import PprCore         ()
import PprCmmExpr      ( pprExpr )
import SrcLoc
import UniqFM
import Util
import Var             ( Var, varType )

import Compiler.Hoopl

import Control.Monad   ( foldM, forM_, void, when, ap )
import Control.Applicative ( Applicative(..) )

import Data.Char       ( ord)
import Data.Maybe
import Data.List     ( minimumBy, nubBy )
import Data.Ord      ( comparing )
import Data.List       ( find, minimumBy )
import Data.Ord        ( comparing )
import qualified Data.Map as Map
import Data.Word       ( Word8, Word16 )

import Foreign.ForeignPtr

#define EVENTLOG_CONSTANTS_ONLY
#include "../../includes/rts/EventLogFormat.h"

-- | Debug information about a block of code. Ticks scope over nested
-- blocks.
data DebugBlock =
  DebugBlock
  { dblProcedure  :: !Label        -- ^ Entry label of containing proc
  , dblLabel      :: !Label        -- ^ Hoopl label
  , dblCLabel     :: !CLabel       -- ^ Output label
  , dblHasInfoTbl :: !Bool         -- ^ Has an info table?
  , dblTicks      :: ![CmmTickish] -- ^ Ticks defined in this block
  , dblSourceTick
            :: !(Maybe CmmTickish) -- ^ Best source tick covering block
  , dblPosition   :: !(Maybe Int)  -- ^ Output position relative to
                                   -- other blocks. @Nothing@ means
                                   -- the block was optimized out
  , dblUnwind     :: !UnwindTable  -- ^ Unwind information
  , dblBlocks     :: ![DebugBlock] -- ^ Nested blocks
  }

-- | Is this the entry block?
dblIsEntry :: DebugBlock -> Bool
dblIsEntry blk = dblProcedure blk == dblLabel blk

instance Outputable DebugBlock where
  ppr blk = (if dblProcedure blk == dblLabel blk
             then text "proc "
             else if dblHasInfoTbl blk
                  then text "pp-blk "
                  else text "blk ") <>
            ppr (dblLabel blk) <+> parens (ppr (dblCLabel blk)) <+>
            (maybe empty ppr (dblSourceTick blk)) <+>
            (maybe (text "removed") ((text "pos " <>) . ppr)
                   (dblPosition blk)) <+>
            pprUwMap (dblUnwind blk) $$
            (if null (dblBlocks blk) then empty else ppr (dblBlocks blk))
    where pprUw (g, expr) = ppr g <> char '=' <> ppr expr
          pprUwMap = braces . hsep . punctuate comma . map pprUw . Map.toList

-- | Intermediate data structure holding debug-relevant context information
-- about a block.
type BlockContext = (CmmBlock, RawCmmDecl, UnwindTable)

-- | Extract debug data from a group of procedures. We will prefer
-- source notes that come from the given module (presumably the module
-- that we are currently compiling).
cmmDebugGen :: ModLocation -> RawCmmGroup -> [DebugBlock]
cmmDebugGen modLoc decls = map (blocksForScope Nothing) topScopes
  where
      blockCtxs :: Map.Map CmmTickScope [BlockContext]
      blockCtxs = blockContexts decls

      -- Analyse tick scope structure: Each one is either a top-level
      -- tick scope, or the child of another.
      (topScopes, childScopes)
        = splitEithers $ map (\a -> findP a a) $ Map.keys blockCtxs
      findP tsc GlobalScope = Left tsc -- top scope
      findP tsc scp | scp' `Map.member` blockCtxs = Right (scp', tsc)
                    | otherwise                   = findP tsc scp'
        where -- Note that we only following the left parent of
              -- combined scopes. This loses us ticks, which we will
              -- recover by copying ticks below.
              scp' | SubScope _ scp' <- scp      = scp'
                   | CombinedScope scp' _ <- scp = scp'
                   | otherwise                   = panic "findP impossible"

      scopeMap = foldr (uncurry insertMulti) Map.empty childScopes

      -- This allows us to recover ticks that we lost by flattening
      -- the graph. Basically, if the parent is A but the child is
      -- CBA, we know that there is no BA, because it would have taken
      -- priority - but there might be a B scope, with ticks that
      -- would not be associated with our child anymore. Note however
      -- that there might be other childs (DB), which we have to
      -- filter out.
      --
      -- We expect this to be called rarely, which is why we are not
      -- trying too hard to be efficient here. In many cases we won't
      -- have to construct blockCtxsU in the first place.
      ticksToCopy :: CmmTickScope -> [CmmTickish]
      ticksToCopy (CombinedScope scp s) = go s
        where go s | scp `isTickSubScope` s   = [] -- done
                   | SubScope _ s' <- s       = ticks ++ go s'
                   | CombinedScope s1 s2 <- s = ticks ++ go s1 ++ go s2
                   | otherwise                = panic "ticksToCopy impossible"
                where ticks = bCtxsTicks $ fromMaybe [] $ Map.lookup s blockCtxs
      ticksToCopy _ = []
      bCtxsTicks = concatMap (blockTicks . fstOf3)

      -- Finding the "best" source tick is somewhat arbitrary -- we
      -- select the first source span, while preferring source ticks
      -- from the same source file.  Furthermore, dumps take priority
      -- (if we generated one, we probably want debug information to
      -- refer to it).
      bestSrcTick = minimumBy (comparing rangeRating)
      rangeRating (SourceNote span _)
        | srcSpanFile span == thisFile = 1
        | otherwise                    = 2 :: Int
      rangeRating note                 = pprPanic "rangeRating" (ppr note)
      thisFile = maybe nilFS mkFastString $ ml_hs_file modLoc

      -- Returns block tree for this scope as well as all nested
      -- scopes. Note that if there are multiple blocks in the (exact)
      -- same scope we elect one as the "branch" node and add the rest
      -- as children.
      blocksForScope :: Maybe CmmTickish -> CmmTickScope -> DebugBlock
      blocksForScope cstick scope = mkBlock True (head bctxs)
        where bctxs = fromJust $ Map.lookup scope blockCtxs
              nested = fromMaybe [] $ Map.lookup scope scopeMap
              childs = map (mkBlock False) (tail bctxs) ++
                       map (blocksForScope stick) nested
              mkBlock top (block, prc, unwind)
                = DebugBlock { dblProcedure    = g_entry graph
                             , dblLabel        = label
                             , dblCLabel       = case info of
                                 Just (Statics infoLbl _)   -> infoLbl
                                 Nothing
                                   | g_entry graph == label -> entryLbl
                                   | otherwise              -> blockLbl label
                             , dblHasInfoTbl   = isJust info
                             , dblTicks        = ticks
                             , dblPosition     = Nothing -- see cmmDebugLink
                             , dblUnwind       = unwind
                             , dblSourceTick   = stick
                             , dblBlocks       = blocks
                             }
                where (CmmProc infos entryLbl _ graph) = prc
                      label = entryLabel block
                      info = mapLookup label infos
                      blocks | top       = seqList childs childs
                             | otherwise = []

              -- A source tick scopes over all nested blocks. However
              -- their source ticks might take priority.
              isSourceTick SourceNote {} = True
              isSourceTick _             = False
              -- Collect ticks from all blocks inside the tick scope.
              -- We attempt to filter out duplicates while we're at it.
              ticks = nubBy (flip tickishContains) $
                      bCtxsTicks bctxs ++ ticksToCopy scope
              stick = case filter isSourceTick ticks of
                []     -> cstick
                sticks -> Just $! bestSrcTick (sticks ++ maybeToList cstick)

-- | Build a map of blocks sorted by their tick scopes
--
-- This involves a pre-order traversal, as we want blocks in rough
-- control flow order (so ticks have a chance to be sorted in the
-- right order). We also use this opportunity to have blocks inherit
-- unwind information from their predecessor blocks where it is
-- lacking.
blockContexts :: RawCmmGroup -> Map.Map CmmTickScope [BlockContext]
blockContexts decls = Map.map reverse $ foldr walkProc Map.empty decls
  where walkProc CmmData{}                 m = m
        walkProc prc@(CmmProc _ _ _ graph) m
          | mapNull blocks = m
          | otherwise      = snd $ walkBlock prc entry Map.empty (emptyLbls, m)
          where blocks = toBlockMap graph
                entry  = [mapFind (g_entry graph) blocks]
                emptyLbls = setEmpty :: LabelSet
        walkBlock _   []             _      c            = c
        walkBlock prc (block:blocks) unwind (visited, m)
          | lbl `setMember` visited
          = walkBlock prc blocks unwind (visited, m)
          | otherwise
          = walkBlock prc blocks unwind $
            walkBlock prc succs unwind'
              (lbl `setInsert` visited,
               insertMulti scope (block, prc, unwind') m)
          where CmmEntry lbl scope = firstNode block
                unwind' = extractUnwind block `Map.union` unwind
                (CmmProc _ _ _ graph) = prc
                succs = map (flip mapFind (toBlockMap graph))
                            (successors (lastNode block))
        mapFind = mapFindWithDefault (error "contextTree: block not found!")

insertMulti :: Ord k => k -> a -> Map.Map k [a] -> Map.Map k [a]
insertMulti k v = Map.insertWith (const (v:)) k [v]

cmmDebugLabels :: (i -> Bool) -> GenCmmGroup d g (ListGraph i) -> [Label]
cmmDebugLabels isMeta nats = seqList lbls lbls
  where -- Find order in which procedures will be generated by the
        -- back-end (that actually matters for DWARF generation).
        --
        -- Note that we might encounter blocks that are missing or only
        -- consist of meta instructions -- we will declare them missing,
        -- which will skip debug data generation without messing up the
        -- block hierarchy.
        lbls = map blockId $ filter (not . allMeta) $ concatMap getBlocks nats
        getBlocks (CmmProc _ _ _ (ListGraph bs)) = bs
        getBlocks _other                         = []
        allMeta (BasicBlock _ instrs) = all isMeta instrs

-- | Sets position fields in the debug block tree according to native
-- generated code.
cmmDebugLink :: [Label] -> [DebugBlock] -> [DebugBlock]
cmmDebugLink labels blocks = map link blocks
  where blockPos :: LabelMap Int
        blockPos = mapFromList $ flip zip [0..] labels
        link block = block { dblPosition = mapLookup (dblLabel block) blockPos
                           , dblBlocks   = map link (dblBlocks block)
                           }

-- | Converts debug blocks into a label map for easier lookups
debugToMap :: [DebugBlock] -> LabelMap DebugBlock
debugToMap = mapUnions . map go
   where go b = mapInsert (dblLabel b) b $ mapUnions $ map go (dblBlocks b)

-- | Maps registers to expressions that yield their "old" values
-- further up the stack. Most interesting for the stack pointer Sp,
-- but might be useful to document saved registers, too.
type UnwindTable = Map.Map GlobalReg UnwindExpr

-- | Expressions, used for unwind information
data UnwindExpr = UwConst Int                   -- ^ literal value
                | UwReg GlobalReg Int           -- ^ register plus offset
                | UwDeref UnwindExpr            -- ^ pointer dereferencing
                | UwPlus UnwindExpr UnwindExpr
                | UwMinus UnwindExpr UnwindExpr
                | UwTimes UnwindExpr UnwindExpr
                deriving (Eq)

instance Outputable UnwindExpr where
  pprPrec _ (UwConst i)     = ppr i
  pprPrec _ (UwReg g 0)     = ppr g
  pprPrec p (UwReg g x)     = pprPrec p (UwPlus (UwReg g 0) (UwConst x))
  pprPrec _ (UwDeref e)     = char '*' <> pprPrec 3 e
  pprPrec p (UwPlus e0 e1)  | p <= 0
                            = pprPrec 0 e0 <> char '+' <> pprPrec 0 e1
  pprPrec p (UwMinus e0 e1) | p <= 0
                            = pprPrec 1 e0 <> char '-' <> pprPrec 1 e1
  pprPrec p (UwTimes e0 e1) | p <= 1
                            = pprPrec 2 e0 <> char '*' <> pprPrec 2 e1
  pprPrec _ other           = parens (pprPrec 0 other)

extractUnwind :: CmmBlock -> UnwindTable
extractUnwind b = go $ blockToList mid
  where (_, mid, _) = blockSplit b
        go :: [CmmNode O O] -> UnwindTable
        go []       = Map.empty
        go (x : xs) = case x of
          CmmUnwind g so -> Map.insert g (toUnwindExpr so) $! go xs
          CmmTick {}     -> go xs
          _other         -> Map.empty
                            -- TODO: Unwind statements after actual instructions

-- | Conversion of Cmm expressions to unwind expressions. We check for
-- unsupported operator usages and simplify the expression as far as
-- possible.
toUnwindExpr :: CmmExpr -> UnwindExpr
toUnwindExpr (CmmLit (CmmInt i _))       = UwConst (fromIntegral i)
toUnwindExpr (CmmRegOff (CmmGlobal g) i) = UwReg g i
toUnwindExpr (CmmReg (CmmGlobal g))      = UwReg g 0
toUnwindExpr (CmmLoad e _)               = UwDeref (toUnwindExpr e)
toUnwindExpr e@(CmmMachOp op [e1, e2])   =
  case (op, toUnwindExpr e1, toUnwindExpr e2) of
    (MO_Add{}, UwReg r x, UwConst y) -> UwReg r (x + y)
    (MO_Sub{}, UwReg r x, UwConst y) -> UwReg r (x - y)
    (MO_Add{}, UwConst x, UwReg r y) -> UwReg r (x + y)
    (MO_Add{}, UwConst x, UwConst y) -> UwConst (x + y)
    (MO_Sub{}, UwConst x, UwConst y) -> UwConst (x - y)
    (MO_Mul{}, UwConst x, UwConst y) -> UwConst (x * y)
    (MO_Add{}, u1,        u2       ) -> UwPlus u1 u2
    (MO_Sub{}, u1,        u2       ) -> UwMinus u1 u2
    (MO_Mul{}, u1,        u2       ) -> UwTimes u1 u2
    _otherwise -> pprPanic "Unsupported operator in unwind expression!"
                           (pprExpr e)
toUnwindExpr e
  = pprPanic "Unsupported unwind expression!" (ppr e)

-- | Generates debug data into a buffer
writeDebugToEventlog :: DynFlags -> ModLocation -> [DebugBlock] -> IO (Int, ForeignPtr Word8)
writeDebugToEventlog dflags mod_loc blocks = do

  -- Write data into a binary memory handle
  bh <- openBinMem $ 1024 * 1024
  let code = do putEvent EVENT_DEBUG_MODULE $ do
                  putString $ packageKeyString (thisPackage dflags)
                  putString $ fromMaybe "???" $ ml_hs_file mod_loc
                foldM (putBlock maxBound) 0 blocks
  void $ runPutDbg code bh dflags emptyUFM
  getBinMemBuf bh

-- | Packs the given static value into a (variable-length) event-log
-- packet.
putEvent :: Word8 -> PutDbgM () -> PutDbgM ()
putEvent id cts
  = PutDbgM $ \bh df cm ->
     let wrap = do
           put_ bh id
           -- Put placeholder for size
           sizePos <- put bh (0 :: Word16)
           -- Put contents
           res <- runPutDbg cts bh df cm
           -- Put final size
           endPos <- tellBin bh
           putAt bh sizePos $ fromIntegral $ (endPos `diffBin` sizePos) - 2
           -- Seek back
           seekBin bh endPos
           return res
     in do catchSize bh 0x10000 wrap (return (cm, ()))

-- | Puts an alternate version if the first one is bigger than the
-- given limit.
--
-- This is a pretty crude way of handling oversized
-- packets... Can't think of a better way right now though.
catchSize :: BinHandle -> Int -> IO a -> IO a -> IO a
catchSize bh limit cts1 cts2 = do

  -- Put contents, note how much size it uses
  start <- tellBin bh :: IO (Bin ())
  a <- cts1
  end <- tellBin bh

  -- Seek back and put second version if size is over limit
  if (end `diffBin` start) >= limit
    then seekBin bh start >> cts2
    else return a

type BlockId = Word16

putBlock :: BlockId -> BlockId -> DebugBlock -> PutDbgM BlockId
putBlock pid bid block = do
  -- Put sub-blocks
  bid' <- foldM (putBlock bid) (bid+1) (dblBlocks block)
  -- Write our own data
  putEvent EVENT_DEBUG_BLOCK $ do
    putDbg bid
    putDbg pid
    dflags <- getDynFlags
    let showSDocC = flip (renderWithStyle dflags) (mkCodeStyle CStyle)
    putString $ showSDocC $ ppr $ dblCLabel block
  -- Write annotations.
  mapM_ putAnnotEvent (dblTicks block)
  return bid'

putAnnotEvent :: RawTickish -> PutDbgM ()
putAnnotEvent (SourceNote ss names) =
  putEvent EVENT_DEBUG_SOURCE $ do
    putDbg $ encLoc $ srcSpanStartLine ss
    putDbg $ encLoc $ srcSpanStartCol ss
    putDbg $ encLoc $ srcSpanEndLine ss
    putDbg $ encLoc $ srcSpanEndCol ss
    putString $ unpackFS $ srcSpanFile ss
    putString names
 where encLoc x = fromIntegral x :: Word16

putAnnotEvent (CoreNote lbl corePtr)
  -- This piece of core was already covered earlier in this block?
  = do elem <- elemCoreMap (lbl, exprPtrCons corePtr)
       when (not elem) $ putEvent EVENT_DEBUG_CORE $ do
         dflags <- getDynFlags
         putString $ showSDocDump dflags $ ppr lbl
         -- Emit core, leaving out (= referencing) any core pieces
         -- that were emitted from sub-blocks
         case corePtr of
           ExprPtr core -> putCoreExpr core >> addToCoreMap lbl DEFAULT
           AltPtr  alt  -> putCoreAlt alt >> addToCoreMap lbl (fstOf3 alt)

putAnnotEvent _ = return ()

-- | Constants for core binary representation
coreMisc, coreApp, coreRef, coreLam, coreLet, coreCase, coreAlt :: Word8
coreMisc = 0
coreApp  = 1
coreRef  = 2
coreLam  = 3
coreLet  = 4
coreCase = 5
coreAlt  = 6

type CoreMap = UniqFM [AltCon]
newtype PutDbgM a = PutDbgM { runPutDbg :: BinHandle -> DynFlags -> CoreMap -> IO (CoreMap, a) }

instance Functor PutDbgM where
  fmap f m = PutDbgM $ \bh df cm -> runPutDbg m bh df cm >>= \(cm', a) -> return (cm', f a)
instance Monad PutDbgM where
  return x = PutDbgM $ \_ _ cm -> return (cm, x)
  m >>= f = PutDbgM $ \bh df cm -> runPutDbg m bh df cm >>= \(cm', a) -> runPutDbg (f a) bh df cm'
instance Applicative PutDbgM where
  pure = return
  (<*>) = ap

instance HasDynFlags PutDbgM where
  getDynFlags = PutDbgM $ \_ df cm -> return (cm,df)

putDbg :: Binary a => a -> PutDbgM ()
putDbg x = PutDbgM $ \bf _ cm -> put_ bf x >> return (cm,())

-- | Put a C-style string (null-terminated). We assume that the string
-- is ASCII.
--
-- This could well be subject to change in future...
putString :: String -> PutDbgM ()
putString str = do
  let putByte = putDbg :: Word8 -> PutDbgM ()
  mapM_ (putByte . fromIntegral . ord) str
  putByte 0

putSDoc :: SDoc -> PutDbgM ()
putSDoc str = do
  dflags <- getDynFlags
  putString $ showSDoc dflags str

elemCoreMap :: (Var, AltCon) -> PutDbgM Bool
elemCoreMap (bind, con) = PutDbgM $ \_ _ cm -> return $ (,) cm $
  case lookupUFM cm bind of
    Just cs -> con `elem` cs
    Nothing -> False

addToCoreMap :: Var -> AltCon -> PutDbgM ()
addToCoreMap b a = PutDbgM $ \_ _ cm -> return (addToUFM_C (\o _ -> a:o) cm b [a], ())

putCoreExpr :: CoreExpr -> PutDbgM ()
putCoreExpr (App e1 e2) = do
  putDbg coreApp
  putCoreExpr e1
  putCoreExpr e2
putCoreExpr (Lam b e) = do
  putDbg coreLam
  putSDoc $ ppr b
  putSDoc $ ppr $ varType b
  putCoreExpr e
putCoreExpr (Let es e) = do
  putDbg coreLet
  putCoreLet es
  putCoreExpr e
putCoreExpr (Case expr bind _ alts) = do
  putDbg coreCase
  putCoreExpr expr
  putSDoc $ ppr bind
  putSDoc $ ppr $ varType bind
  putDbg (fromIntegral (length alts) :: Word16)
  forM_ alts $ \alt@(a, _, _) ->
    checkCoreRef (bind, a) $
      putCoreAlt alt
putCoreExpr (Cast e _) = putCoreExpr e
putCoreExpr (Tick _ e) = putCoreExpr e
-- All other elements are supposed to have a simple "pretty printed"
-- representation that we can simply output verbatim.
putCoreExpr other = do
  putDbg coreMisc
  putSDoc $ ppr other

putCoreAlt :: CoreAlt -> PutDbgM ()
putCoreAlt (a,binds,e) = do
  putDbg coreAlt
  putSDoc $ case a of
    DEFAULT -> empty
    _       -> ppr a
  putDbg (fromIntegral (length binds) :: Word16)
  forM_ binds $ \b -> do
    putSDoc . ppr $ b
    putSDoc . ppr . varType $ b
  putCoreExpr e

putCoreLet :: CoreBind -> PutDbgM ()
putCoreLet (NonRec b e) = do
  putDbg (1 :: Word16) -- could use 0 to mark non-recursive case?
  putSDoc $ ppr b
  putSDoc $ ppr $ varType b
  checkCoreRef (b, DEFAULT) $ putCoreExpr e
putCoreLet (Rec ps) = do
  putDbg (fromIntegral $ length ps :: Word16)
  forM_ ps $ \(b, e) -> do
    putSDoc $ ppr b
    putSDoc $ ppr $ varType b
    checkCoreRef (b, DEFAULT) $ putCoreExpr e

-- | Generate reference to core piece that was output elsewhere... Or
-- proceed with given code otherwise.
checkCoreRef :: (Var, AltCon) -> PutDbgM () -> PutDbgM ()
checkCoreRef (b,a) code = do
  elem <- elemCoreMap (b,a)
  if elem
    then do putDbg coreRef
            dflags <- getDynFlags
            putString $ showSDocDump dflags $ ppr b
            putSDoc $ case a of
              DEFAULT -> empty
              _       -> ppr a
    else do addToCoreMap b a
            code
