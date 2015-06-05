/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team 2014-2014
 *
 * Prototypes for functions in Stack.c 
 * (Functions operating on the STG stack)
 *
 * -------------------------------------------------------------------------*/

#ifndef PUBLIC_STACK_H
#define PUBLIC_STACK_H

/* reifyStack */
StgArrWords *reifyStack (Capability* cap, StgPtr sp);

#endif /* STACK_H */
