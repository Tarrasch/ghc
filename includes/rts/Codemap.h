#ifndef PUBLIC_CODEMAP_H
#define PUBLIC_CODEMAP_H
/* ----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2015-2015
 *
 * Codemap
 *
 * This module tries to give a mapping between code instruction pointer within
 * the running binary (argv[0]) to a descriptive source file location info.
 *
 * Essentially, this is like using a debugger like gdb but reimplemented in the
 * rts.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   http://ghc.haskell.org/trac/ghc/wiki/Commentary/SourceTree/Includes
 *
 * -------------------------------------------------------------------------- */

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

// Before using codemap_lookup_ip, you must load the debug data. Loading means
// to read and parse the debug data from the binary currently running (argv[0]).
//
// Use these functions to load and unload codemap, these methods are
// synchronized if THREADED_RTS is set. Using codemap_inc_ref() and
// the other synchronized methods is the recommended way.
void codemap_inc_ref(void); // Will load codemap, if it isn't already.
void codemap_dec_ref(void); // Let rts know your thread is done using codemap.
                            // So it's safe to unload codemap.
StgBool codemap_try_unload(void); // Returns true iff it actually did unload
StgBool codemap_is_loaded(void); // Check if codemap is loaded.

// Codemap must be loaded before using this.
void codemap_lookup_ip(void *ip,
        CodemapProc **p_proc, /* out */
        CodemapUnit **p_unit /* out */
        );

#endif // PUBLIC_CODEMAP_H
