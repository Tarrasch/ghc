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

/* -----------------------------------------------------------------------------
   countStackSize

   Count number of frames on the whole stack. O(n) in time as it will traverse
   the stack.

   @param p Pointer to the stack, it could typically be `my_tso->stackobj->sp`
   -------------------------------------------------------------------------- */
StgWord
countStackSize (StgPtr p)
{
    const StgRetInfoTable* ret_info;
    StgWord framecount;
    framecount = 0;

    while((ret_info = get_ret_itbl((StgClosure *)p)) &&
          ret_info->i.type != STOP_FRAME) {
        framecount++;

        if (ret_info->i.type == UNDERFLOW_FRAME)
            p = ((StgUnderflowFrame*)p)->next_chunk->sp;
        else
            p += stack_frame_sizeW((StgClosure *)p);
    }
    return framecount;
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
    framecount = countStackSize(sp);

    // Allocate array of that size. The length will be stored in
    // the StgArrWords, so we don't need any terminators
    reified = stgAllocArrWords(cap, framecount * sizeof(char *));
    reified_payload = (StgFunPtr*)reified->payload;

    // Crawl the stack again, but this time filling in the
    // newly-allocated array
    StgPtr p = sp;
    while((ret_info = get_ret_itbl((StgClosure *)p)) &&
          ret_info->i.type != STOP_FRAME) {
#if defined(TABLES_NEXT_TO_CODE)
        if( ((StgClosure *)p)->header.info == &stg_upd_frame_info) {
          // In this case, it's more intersting to point to the function that
          // the update frame is going to execute
          *(reified_payload++) = (StgFunPtr)(((StgUpdateFrame*)p)->updatee->header.info);
        }
        else {
          *(reified_payload++) = *(StgFunPtr *)p;
        }
#else
        *(reified_payload++) = ret_info->i.entry;
#endif
        if (ret_info->i.type == UNDERFLOW_FRAME)
            p = ((StgUnderflowFrame*)p)->next_chunk->sp;
        else
            p += stack_frame_sizeW((StgClosure *)p);
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

        // Put skips
        if (num_skipped > 0) {
            debugBelch("     ... %d unknown frames ...\n", num_skipped);
            num_skipped = 0;
        }

        // Put header
        if (!putHeader) {
            debugBelch("Stack trace:\n");
            putHeader = 1;
        }

        // Find debug info
        DebugInfo infos[MAX_DEBUG_INFOS];
        StgWord infoCount = dwarf_get_debug_info(unit, proc, infos, MAX_DEBUG_INFOS);
        if (infoCount == 0) {
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
