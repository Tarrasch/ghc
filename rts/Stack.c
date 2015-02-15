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
// See countLimitedStackSize() for example usage.
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
   countLimitedStackSize

   Count number of frames on the whole stack.

   @param p     Pointer to the stack, typically `my_tso->stackobj->sp`
   @param limit Stop search after limit frames and return limit
   -------------------------------------------------------------------------- */
StgWord
countLimitedStackSize (StgPtr p, const nat limit)
{
    const StgRetInfoTable* ret_info;
    StgWord framecount;
    framecount = 0;
    TRAVERSE_STACK(p, ret_info) {
        framecount++;
        if(framecount >= limit) {
          return framecount;
        }
    }
    return framecount;
}

/* -----------------------------------------------------------------------------
   countStackSize

   Count number of frames on the whole stack. O(stack_size) in time as it will
   traverse the stack.

   @param p     Pointer to the stack, typically `my_tso->stackobj->sp`
   -------------------------------------------------------------------------- */
StgWord
countStackSize (StgPtr p)
{
    const nat maxNatValue = 4294967295;
    return countLimitedStackSize(p, maxNatValue);
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
#if defined(TABLES_NEXT_TO_CODE)
    return *(StgFunPtr *)p;
#else
    return get_ret_itbl(p)->i.entry;
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
  if(RtsFlags.StackTraceFlags.doReify) {
    const StgRetInfoTable* ret_info;
    StgWord framecount;
    StgArrWords* reified;
    StgFunPtr *reified_payload;

    framecount = countLimitedStackSize(sp, RtsFlags.StackTraceFlags.numFrames);
    reified = stgAllocArrWords(cap, framecount * sizeof(char *));
    reified_payload = (StgFunPtr*)reified->payload;

    StgPtr p = sp;
    StgWord count = 0;
    TRAVERSE_STACK (p, ret_info) {
        *(reified_payload++) = getExecuteableCode((StgClosure*)p);
        if (++count >= framecount) {
          break;
        }
    }
    return reified;
  } else {
    return stgAllocArrWords(cap, 0);
  }
}
