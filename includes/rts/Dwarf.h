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

void dwarf_ensure_init(void);
void dwarf_free(void);
StgWord dwarf_lookup_ip(void *ip, DwarfUnit** p_unit, DebugInfo *infos, int max_num_infos);
StgWord dwarf_addr_num_infos(void *ip);

#endif // DWARF2_H
