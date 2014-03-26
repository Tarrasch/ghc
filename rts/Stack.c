/* ---------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2014-2014
 *
 * Stack-related functionality
 *
 * --------------------------------------------------------------------------*/

#include "Rts.h"
#include "RtsUtils.h"
#include "Stack.h"
#include "Printer.h"

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

// It is quite cumbersome to traverse the stack manually, as it's in
// fact chunked. This macro abstracts away from that.
//
// See countStackSize() for an example usage.
#define TRAVERSE_STACK(sp_var, ret_info_var)                                 \
            for (;                                                           \
                 (ret_info_var = get_ret_itbl((StgClosure *)sp_var)) &&      \
                 ret_info_var->i.type != STOP_FRAME                          \
                 ;                                                           \
                 (ret_info_var->i.type == UNDERFLOW_FRAME)                   \
                   ? (sp_var = ((StgUnderflowFrame*)sp_var)->next_chunk->sp) \
                   : (sp_var += stack_frame_sizeW((StgClosure *)sp_var))     \
                )                                                            \

/* -----------------------------------------------------------------------------
   countStackSize

   Count number of frames on the whole stack. O(n) in time as it will traverse
   the stack.

   @param p Pointer to the stack, it could typically be `my_tso->stackobj->sp`
   -------------------------------------------------------------------------- */
StgWord
countLimitedStackSize (StgPtr p, const int limit)
{
    const StgRetInfoTable* ret_info;
    StgWord framecount;
    framecount = 0;
    TRAVERSE_STACK(p, ret_info) {
        framecount++;
        if(limit >= 0 && framecount >= limit) {
          break;
        }
    }
    return framecount;
}

StgWord
countStackSize (StgPtr p)
{
    return countLimitedStackSize(p, -1);
}


/* -----------------------------------------------------------------------------
   getExecuteableCode

   Given a closure object, return a pointer to the executeable code of
   its info table. In the case of it being some sort of an update frame,
   then try to return the code of the updatee rather than the code of
   the update frame.
   -------------------------------------------------------------------------- */
StgFunPtr
getExecuteableCode (StgClosure *p) {
    if (p->header.info == &stg_upd_frame_info) {
        // In this case, it's more intersting to point to the function that
        // the update frame is going to execute
        p = ((StgUpdateFrame*)p)->updatee;
    }
    else if (p->header.info == &stg_bh_upd_frame_info) {
        p = ((StgUpdateFrame*)p)->updatee;
        return ((StgBhInd*)(p))->original_code;
    }
    else if (p->header.info == &stg_artificial_stack_frame_info) {
        debugBelch("Wihii! :)\n\n");
        return (StgFunPtr)(p->payload[0]);
    }
#if defined(TABLES_NEXT_TO_CODE)
    return *(StgFunPtr *)p;
#else
    return get_ret_itbl(p)->i.entry; // (Untested! Not even sure compiles)
#endif
}

/* -----------------------------------------------------------------------------
   reifyStack

   This function is called by the raise# primitve, to reify the STG stack as an
   array of code pointers
   -------------------------------------------------------------------------- */

StgArrWords *
reifyStack (Capability *cap, StgPtr sp)
{
  if(RtsFlags.StackTraceFlags.doAnything) {
    const StgRetInfoTable* ret_info;
    StgWord framecount;
    StgArrWords* reified;
    StgFunPtr *reified_payload;

    // Determine the length of the array we need to allocate to
    // store the stack frame pointer array
    framecount = countLimitedStackSize(sp, RtsFlags.StackTraceFlags.numFrames);

    // Allocate array of that size. The length will be stored in
    // the StgArrWords, so we don't need any terminators
    reified = stgAllocArrWords(cap, framecount * sizeof(char *));
    reified_payload = (StgFunPtr*)reified->payload;

    // Crawl the stack again, but this time filling in the
    // newly-allocated array
    StgPtr p = sp;
    StgWord count = 0;
    TRAVERSE_STACK (p, ret_info) {
        *(reified_payload++) = getExecuteableCode((StgClosure*)p);
        if (++count >= framecount) {
          break;
        }
    }
    dumpStackStructure(cap, sp);
    return reified;
  } else {
    return stgAllocArrWords(cap, 0);
  }
}

void
dumpStackStructure (Capability *cap, StgPtr sp)
{
    StgPtr p = sp;
    const StgRetInfoTable* ret_info;
    while((ret_info = get_ret_itbl((StgClosure *)p)) &&
          ret_info->i.type != STOP_FRAME &&
          ret_info->i.type != UNDERFLOW_FRAME) {
        p += stack_frame_sizeW((StgClosure *)p);
    }
    printStackChunk( sp, p);
}


/* -----------------------------------------------------------------------------
   dumpStack

   Dumps a reified stack to the console.
   -------------------------------------------------------------------------- */

#define MAX_DEBUG_INFOS 100

void
dumpStack (StgArrWords *stack)
{

#ifdef USE_DWARF

    // Load dwarf data
    debugBelch("Loading debug data...\n");
    dwarf_load();
    dwarf_init_lookup();

    // Pointer count
    StgFunPtr *ptrs = (StgFunPtr *)stack->payload;
    StgWord ptrCount = stack->bytes / sizeof(void*);
    StgWord i;
    StgBool putHeader = 0;
    int num_skipped = 0;
    for (i = 0; i < ptrCount; i++) {

        // Lookup PC
        DwarfUnit *unit;
        DwarfProc *proc = dwarf_lookup_proc(ptrs[i], &unit);
        if (!proc) {
            num_skipped++;
            continue;
        }

        // Put header
        if (!putHeader) {
            debugBelch("Stack trace:\n");
            putHeader = 1;
        }

        // Put skips
        if (num_skipped > 0) {
            debugBelch("   -- ... %d unknown frames ...\n", num_skipped);
            num_skipped = 0;
        }

        // Find debug info
        DebugInfo infos[MAX_DEBUG_INFOS];
        StgWord infoCount = dwarf_get_debug_info(unit, proc, infos, MAX_DEBUG_INFOS);
        if (infoCount == 0) { // XXX ARASH  TAMPERED to >=
            debugBelch("%4lu: %s (using %s)\n", i, proc->name, unit->name);
            continue;
        }

        // Write what we know
        StgWord j;
        for (j = 0; j < infoCount; j++) {
            if (j == 0) debugBelch("%4lu: ", i); else debugBelch("      ");
            debugBelch("%s (at %s:%d:%d-%d:%d)\n",
                       infos[j].name, infos[j].file,
                       infos[j].sline, infos[j].scol, infos[j].eline, infos[j].ecol);
        }
    }

    // Clean up
    dwarf_free();

#endif

}
