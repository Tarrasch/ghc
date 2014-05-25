#ifndef DWARF_H
#define DWARF_H

// Since you must be able to see this from the base library in Haskell-land,
// we can't guard on USE_DWARF (But the c-file can and should)
// #ifdef USE_DWARF

// #include "Hash.h"

typedef struct hashtable HashTable; /* Forward declare */

typedef struct DwarfUnit_ DwarfUnit;
typedef struct DwarfProc_ DwarfProc;
typedef enum DwarfSource_ DwarfSource;
typedef struct DebugInfo_ DebugInfo;

struct DwarfUnit_ {
	char *name;
	char *comp_dir;
	void *low_pc, *high_pc;
	StgWord8 *debug_data;
	DwarfProc *procs;
	StgWord proc_count;
	StgWord16 max_proc_id;

	HashTable *proc_table; // by name // s/HashTable/void
	DwarfProc **procs_by_id; // by id
	DwarfProc **procs_by_pc; // by low_pc

	DwarfUnit *next;
};

enum DwarfSource_ {
	DwarfSourceDwarf,
	DwarfSourceDwarfBlock,
	DwarfSourceSymtab,
};

struct DwarfProc_ {
	char *name;
	StgWord16 id;
	StgWord16 parent_id;
	void *low_pc;
	void *high_pc;
	StgWord8 *debug_data;
	DwarfSource source;
	struct DwarfProc_ *next;
};

struct DebugInfo_ {
	StgWord16 sline, scol, eline, ecol;
	char *file;
	char *name;
	StgWord depth;
};

extern DwarfUnit *dwarf_units;

// In the dwarf sense, there is two forms of initing dwarf. We use the
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
//  exception handlers are only going to load the dwarf data when we know for
//  sure that we're going to print the stack trace.
//
//  [note -gsplit-dwarf] One discussed optimization is to make it possible to
//  do "half-loading". No work has been started on this as of yet, but some
//  interested discussion can be found in this disussion on github:
//
//    https://github.com/scpmw/ghc/commit/bbf6f35d8c341c8aadca1a48657084c007837b21#commitcomment-5527280
//
//  (Arash May 2014)

// To be called once when the RTS starts.
void initDwarf(void);

// Use these functions to load and unload dwarf, these methods are
// synchronized if THREADED_RTS is set. Using dwarf_inc_ref() and
// the other synchronized methods is the recommended way
void dwarf_inc_ref(void);
void dwarf_dec_ref(void);
StgBool dwarf_try_unload(void); // Returns true iff it actually did unload

// These methods can be called many times once dwarf is loaded. However, the
// callers must be sure that the dwarf data actually is loaded when called and
// don't gets unloaded while the method is running. It is therefore a good idea
// to do dwarf_inc_ref() before using these functions.
DwarfUnit *dwarf_get_unit(char *name);
DwarfProc *dwarf_get_proc(DwarfUnit *unit, char *name);
DwarfProc *dwarf_lookup_proc(void *ip, DwarfUnit **punit);
StgWord dwarf_get_debug_info(DwarfUnit *unit, DwarfProc *proc, DebugInfo *infos, StgWord max_infos);
StgWord dwarf_lookup_ip(void *ip,
    DwarfProc **p_proc, /* in OR out */
    DwarfUnit **p_unit, /* out */
    DebugInfo *infos,   /* out */
    int max_num_infos);
StgWord dwarf_addr_num_infos(void *ip);

// These methods are for loading and unloading without synchronization. You
// probably should use the synchronized methods.
void dwarf_force_load(void);
void dwarf_force_unload(void);


// #endif // USE_DWARF

#endif // DWARF_H
