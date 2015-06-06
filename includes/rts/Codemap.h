#ifndef CODEMAP_H
#define CODEMAP_H

// Since you should be able to see this from base library, we can't guard on
// USE_DWARF (But the c-file can)
#include "Rts.h"

typedef struct hashtable HashTable; /* Forward declare */

typedef struct CodemapUnit_ CodemapUnit;
typedef struct CodemapProc_ CodemapProc;

struct CodemapUnit_ {
    char *name;
    char *comp_dir;
    void *low_pc, *high_pc;
    CodemapProc *procs;
    StgWord proc_count;

    HashTable *proc_table; // by name // s/HashTable/void
    CodemapProc **procs_by_pc; // by low_pc

    CodemapUnit *next;
};

struct CodemapProc_ {
    char *name;
    void *low_pc;
    void *high_pc;
    struct CodemapProc_ *next;
};

extern CodemapUnit *codemap_units;

// In the codemap sense, there is two forms of initing codemap. We use the
// following terminology to distinguish them:
//
//  "initalizing": Only done once when the RTS starts. This is done very fast
//  and will always be done once during the life-time of a Haskell program.
//
//  "loading": Loading means to read and parse the debug data from the binary
//  that contains the Haskell binary, some memory-heavy indexes are also built
//  (so you can quickly lookup the functions in say a stack trace). The whole
//  loading procedure can take seconds. This should therefore be avoided when
//  not necessary. Note that in Haskell, exceptions are common so sensible
//  exception handlers are only going to load the codemap data when we know for
//  sure that we're going to print the stack trace.
//
//  [note -gsplit-dwarf] One discussed optimization is to make it possible to
//  do "half-loading". No work has been started on this as of yet, but some
//  interested discussion can be found in this disussion on github:
//
//    https://github.com/scpmw/ghc/commit/bbf6f35#commitcomment-5527280

// To be called once when the RTS starts.
void initCodemap(void);

// Use these functions to load and unload codemap, these methods are
// synchronized if THREADED_RTS is set. Using codemap_inc_ref() and
// the other synchronized methods is the recommended way
void codemap_inc_ref(void);
void codemap_dec_ref(void);
StgBool codemap_try_unload(void); // Returns true iff it actually did unload
StgBool codemap_is_loaded(void);

// These methods can be called many times once codemap is loaded. However, the
// callers must be sure that the codemap data actually is loaded when called and
// don't gets unloaded while the method is running. It is therefore a good idea
// to do codemap_inc_ref() before using these functions.
CodemapUnit *codemap_get_unit(char *name);
CodemapProc *codemap_get_proc(CodemapUnit *unit, char *name);
CodemapProc *codemap_lookup_proc(void *ip, CodemapUnit **punit);
void codemap_lookup_ip(void *ip,
        CodemapProc **p_proc, /* in OR out */
        CodemapUnit **p_unit /* out */
        );

#endif // CODEMAP_H
