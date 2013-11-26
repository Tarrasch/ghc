#ifndef DWARF2_H
#define DWARF2_H

// This is the public interface in the sense that the Haskell code can access it.

typedef struct DwarfUnit_ DwarfUnit;
typedef struct DebugInfo_ DebugInfo;

struct DebugInfo_ {
	StgWord16 sline, scol, eline, ecol;
	char *file;
	char *name;
	StgWord depth;
};

struct DwarfUnit_ ;


#define PUBLIC_DWARF_UNIT_MEMBERS \
  char *name; \

// This struct contains the "public" portions of a DwarfUnit
//
// Note: Alternatively you can make the whole DwarfUnit (and also DwarfProc
// eventually) public. At that point you can completely erase the other
// "Dwarf.h" and only keep this header-file.
//
// A third option is to have C-functions which just work as getters.
typedef struct PublicDwarfUnit_ {
  PUBLIC_DWARF_UNIT_MEMBERS
} PublicDwarfUnit;



void dwarf_ensure_init(void);
void dwarf_free(void);
StgWord dwarf_lookup_ip(void *ip, DwarfUnit** p_unit, DebugInfo *infos, int max_num_infos);
StgWord dwarf_addr_num_infos(void *ip);

#endif // DWARF2_H
