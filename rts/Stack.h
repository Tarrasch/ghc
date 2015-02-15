/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2014-2014
 *
 * Prototypes for functions in Stack.c 
 * (Functions operating on the STG stack)
 *
 * -------------------------------------------------------------------------*/

#ifndef STACK_H
#define STACK_H

#include "Capability.h"

#include "BeginPrivate.h"

/* countStackSize */
StgWord countStackSize (StgPtr sp);
StgWord countLimitedStackSize (StgPtr sp, int limit);

/* getExecuteableCode */
StgFunPtr getExecuteableCode (StgClosure *p);

/* reifyStack */
StgArrWords *reifyStack (Capability* cap, StgPtr sp);

void dumpStackStructure (Capability *cap, StgPtr sp);

/* Helper to dump a reified stack */
void dumpStack (StgArrWords *stack);

#include "EndPrivate.h"

#endif /* STACK_H */
