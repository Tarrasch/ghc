#ifdef USE_ELF

#include "rts/Elf.h"
#include "Rts.h"
#include "RtsUtils.h"

#include "Hash.h"
#include "Trace.h"

#include "gelf.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>


// Global compilation unit list
DwarfUnit *dwarf_units;

#ifdef THREADED_RTS
// Lock (if multiple threads write to the globals using dwarf_ensure_init())
Mutex dwarf_mutex;
#endif
int dwarf_ref; // When dwarf_ref > 0, then dwarf data should be loaded. But
               // when it is == 0, it may also be loaded.


// Internal helpers
static void dwarf_load_symbols(char *file, Elf *elf);

static void dwarf_load_file(char *module_path);
static void dwarf_init_lookup(void);

static DwarfUnit *dwarf_new_unit(char *name, char *comp_dir);
static DwarfProc *dwarf_new_proc(DwarfUnit *unit, char *name, GElf_Addr low_pc, GElf_Addr high_pc);

void initDwarf() {
  dwarf_units = NULL;
  dwarf_ref = 0;
#ifdef THREADED_RTS
  initMutex(&dwarf_mutex);
#endif
}

int dwarf_is_loaded() {
  return dwarf_units != NULL;
}

void dwarf_force_load()
{
  if (dwarf_is_loaded()) {
    errorBelch("Dwarf is already loaded!");
    return;
  }

  // Initialize ELF library
  if (elf_version(EV_CURRENT) == EV_NONE) {
    errorBelch("libelf version too old!");
    return;
  }

  dwarf_load_file(prog_argv[0]);
  dwarf_init_lookup();
}

void dwarf_load_file(char *module_path)
{

	// Open the module
	int fd = open(module_path, O_RDONLY);
	if (fd < 0) {
		sysErrorBelch("Could not open %s for reading debug data", module_path);
		return;
	}

	// Open using libelf (no archives, don't need elf_next)
	Elf *elf = elf_begin(fd, ELF_C_READ, 0);
	if(!elf) {
		errorBelch("Could not open ELF file: %s", elf_errmsg(-1));
		close(fd);
		return;
	}

	// Not actually an ELF file? That's okay, we are attempting this
	// for pretty much all memory-mapped files, so we can expect to
	// come across a few non-object files.
	if (elf_kind(elf) != ELF_K_ELF) {
		elf_end(elf);
		close(fd);
		return;
	}

	// Load symbols
	dwarf_load_symbols(module_path, elf);

	elf_end(elf);
	close(fd);
}

// Catch-all unit to use where we don't have (or chose to ignore) a
// "file" entry in the symtab
#define SYMTAB_UNIT_NAME "SYMTAB: %s"  // TODO(arash): remove

void dwarf_load_symbols(char *file, Elf *elf)
{
	// Locate symbol table section
	Elf_Scn *scn = 0; GElf_Shdr hdr;
	GElf_Shdr sym_shdr;
	GElf_Half sym_shndx = ~0;
  size_t shstrndx;

  if (elf_getshdrstrndx(elf, &shstrndx) != 0) {
    errorBelch("elf_getshdrstrndx() failed: %s.", elf_errmsg(-1));
  }

	while ((scn = elf_nextscn(elf, scn))) {
		if (!gelf_getshdr(scn, &hdr))
			return;
		if (hdr.sh_type != SHT_SYMTAB && hdr.sh_type != SHT_DYNSYM)
			continue;
		// Get data
		Elf_Data *data = elf_getdata(scn, 0);
		if (!data)
			return;

    // TODO(arash): Why did you do this scpmw??? There exists already a unit
    // name afaict.
		/* // Find or create the catch-all unit for symtab entries */
		/* char symtab_unit_name[1024]; */
		/* snprintf (symtab_unit_name, 1024, SYMTAB_UNIT_NAME, file); */
    char *symtab_unit_name;
    if ((symtab_unit_name = elf_strptr(elf, shstrndx, hdr.sh_name)) == NULL)
      errorBelch("elf_strptr() failed: %s.", elf_errmsg(-1));

		DwarfUnit *unit = dwarf_get_unit(symtab_unit_name);
		if (!unit) unit = dwarf_new_unit(symtab_unit_name, "");

		// Iterate over symbols
		nat ndx;
		for (ndx = 1; ndx < hdr.sh_size / hdr.sh_entsize; ndx++) {

			// Get symbol data
			GElf_Sym sym;
			if (gelf_getsym(data, ndx, &sym) != &sym) {
				errorBelch("DWARF: Could not read symbol %d: %s\n", ndx, elf_errmsg(-1));
				continue;
			}

			// Look up string
			char *name = elf_strptr(elf, hdr.sh_link, sym.st_name);
			if (!name) {
				errorBelch("DWARF: Could not lookup name for symbol no %d: %s\n", ndx, elf_errmsg(-1));
				continue;
			}

			// Load associated section header. Use cached one where
			// applicable.
			if (sym.st_shndx != sym_shndx) {
				if (sym.st_shndx == SHN_ABS) {
					memset(&sym_shdr, 0, sizeof(sym_shdr));
				} else if(sym.st_shndx == SHN_UNDEF) {
					continue;
				} else {

					Elf_Scn *sym_scn = elf_getscn(elf, sym.st_shndx);
					if (gelf_getshdr(sym_scn, &sym_shdr) != &sym_shdr) {
						memset(&sym_shdr, 0, sizeof(sym_shdr));
					}
				}
				sym_shndx = sym.st_shndx;
			}

			// Type?
			switch (GELF_ST_TYPE(sym.st_info)) {

			// Haskell symbols can appear in the symbol table flagged as
			// just about anything.
			case STT_NOTYPE:
			case STT_FUNC:
			case STT_OBJECT:

				// Only look at symbols from executable sections
				if (!(sym_shdr.sh_flags & SHF_EXECINSTR) ||
				    !(sym_shdr.sh_flags & SHF_ALLOC))
					continue;

				// Need a compilation unit to add name to. Ignore
				// unaccounted-for names.
				if (!unit)
					break;

				// Add procedure
				dwarf_new_proc(unit, name,
				               sym.st_value, sym.st_value+sym.st_size
				               );

				break;
			}
		}

	}
}


DwarfUnit *dwarf_get_unit(char *name)
{
	DwarfUnit *unit;
	for (unit = dwarf_units; unit; unit = unit->next)
		if (!strcmp(name, unit->name))
			return unit;
	return 0;
}

DwarfUnit *dwarf_new_unit(char *name, char *comp_dir)
{
	DwarfUnit *unit = (DwarfUnit *)stgMallocBytes(sizeof(DwarfUnit), "dwarf_new_unit");
	unit->name = strdup(name);
	unit->comp_dir = strdup(comp_dir);
	unit->low_pc = NULL;
	unit->high_pc = NULL;
	unit->procs = NULL;
	unit->proc_count = 0;
	unit->proc_table = allocStrHashTable();
	unit->procs_by_pc = NULL;
	unit->next = dwarf_units;
	dwarf_units = unit;
	return unit;
}

DwarfProc *dwarf_get_proc(DwarfUnit *unit, char *name)
{
	return lookupStrHashTable(unit->proc_table, name);
}

DwarfProc *dwarf_new_proc(DwarfUnit *unit, char *name,
                          GElf_Addr low_pc, GElf_Addr high_pc
                          )
{
	// Security
	if (high_pc <= low_pc)
		return NULL;

	DwarfProc *proc = (DwarfProc *)stgMallocBytes(sizeof(DwarfProc), "dwarf_new_proc");
	proc->name = strdup(name);
	proc->low_pc = low_pc;
	proc->high_pc = high_pc;

	proc->next = unit->procs;
	unit->procs = proc;

	// Update unit data
  insertStrHashTable(unit->proc_table, proc->name, proc);
	if (!unit->low_pc || low_pc < unit->low_pc)
		unit->low_pc = low_pc;
	if (!unit->high_pc || high_pc > unit->high_pc)
		unit->high_pc = high_pc;
	unit->proc_count++;

	return proc;
}

void dwarf_force_unload()
{
        if (!dwarf_is_loaded()) {
                errorBelch("Dwarf is not even loaded!");
        }
	DwarfUnit *unit;
	while ((unit = dwarf_units)) {
		dwarf_units = unit->next;
		freeHashTable(unit->proc_table, NULL);
		free(unit->procs_by_pc);

		DwarfProc *proc;
		while ((proc = unit->procs)) {
			unit->procs = proc->next;
			free(proc->name);
			free(proc);
		}

		free(unit->name);
		free(unit->comp_dir);
		free(unit);
	}
}

// Builds up associations between debug and DWARF data.

int compare_low_pc(const void *a, const void *b);
int compare_low_pc(const void *a, const void *b) {
	DwarfProc *proca = *(DwarfProc **)a;
	DwarfProc *procb = *(DwarfProc **)b;
	if (proca->low_pc < procb->low_pc) return -1;
	if (proca->low_pc == procb->low_pc) {
    return 0;
	}
	return 1;
}

// For debbuging purposes
void dwarf_dump_tables(DwarfUnit *unit);
void dwarf_dump_tables(DwarfUnit *unit)
{
	StgWord i;
	printf(" Unit %s (%lu procs) %p-%p:\n",
	       unit->name, unit->proc_count,
	       unit->low_pc, unit->high_pc);
	for (i = 0; i < unit->proc_count; i++) {
		printf("%p-%p: %s\n",
			   unit->procs_by_pc[i]->low_pc, unit->procs_by_pc[i]->high_pc,
			       unit->procs_by_pc[i]->name);
	}
}

void dwarf_init_lookup(void)
{
	// Build procedure tables for every unit
	DwarfUnit *unit;
	for (unit = dwarf_units; unit; unit = unit->next) {

		// Just in case we run this twice for some reason
		free(unit->procs_by_pc); unit->procs_by_pc = NULL;

		// Allocate tables
		StgWord pcTableSize = unit->proc_count * sizeof(DwarfProc *);
		unit->procs_by_pc = (DwarfProc **)stgMallocBytes(pcTableSize, "dwarf_init_pc_table");

		// Populate
		StgWord i = 0;
		DwarfProc *proc;
		for (proc = unit->procs; proc; proc = proc->next) {
			unit->procs_by_pc[i++] = proc;
		}

		// Sort PC table by low_pc
		qsort(unit->procs_by_pc, unit->proc_count, sizeof(DwarfProc *), compare_low_pc);

	}
}

DwarfProc *dwarf_lookup_proc(void *ip, DwarfUnit **punit)
{
	DwarfUnit *unit;
	for (unit = dwarf_units; unit; unit = unit->next) {

		// Pointer in unit range?
		if (ip < unit->low_pc || ip >= unit->high_pc)
			continue;
		if (!unit->proc_count || !unit->procs_by_pc)
			continue;

		// Find first entry with low_pc < ip in table (using binary search)
		StgWord low = 0, high = unit->proc_count;
		while (low < high) {
			int mid = (low + high) / 2;
			if (unit->procs_by_pc[mid]->low_pc <= ip)
				low = mid + 1;
			else
				high = mid;
		}

		// Find an entry covering it
		while (low > 0) {
			DwarfProc *proc = unit->procs_by_pc[low-1];

			// Security
			if (ip < proc->low_pc) {
				debugBelch("DWARF lookup: PC table corruption!");
				break;
			}

			// In bounds? Then we have found it
			if (ip <= proc->high_pc) {
				if (punit)
					*punit = unit;
				return proc;
			}

			/* // Not a block? Stop search */
			/* if (proc->source != DwarfSourceDwarfBlock) */
			/* 	break; */

			// Otherwise backtrack
			low--;
		}

	}

	return NULL;
}

StgWord dwarf_lookup_ip(void *ip, DwarfProc **p_proc, DwarfUnit **p_unit)
{
    *p_proc = dwarf_lookup_proc(ip, p_unit);
    if (*p_proc == NULL) {
        return 0;
    }
    return 1;
}

void dwarf_inc_ref(void) {
        ACQUIRE_LOCK(&dwarf_mutex);
        dwarf_ref++;
        if (!dwarf_is_loaded()) {
                // If isn't initialized
                dwarf_force_load();
        }
        RELEASE_LOCK(&dwarf_mutex);
}

void dwarf_dec_ref(void) {
        ACQUIRE_LOCK(&dwarf_mutex);
        dwarf_ref--;
        RELEASE_LOCK(&dwarf_mutex);
}

StgBool dwarf_try_unload(void) {
        StgBool will_unload;
        ACQUIRE_LOCK(&dwarf_mutex);
        will_unload = dwarf_ref == 0 && dwarf_is_loaded();
        if (will_unload) {
                dwarf_force_unload();
        }
        RELEASE_LOCK(&dwarf_mutex);
        return will_unload;
}

#else /* USE_ELF */

#include "Rts.h"
#include "rts/Elf.h"

StgWord dwarf_lookup_ip(void *ip, DwarfProc **p_proc, DwarfUnit **p_unit)
{
    *p_proc = NULL;
    *p_unit = NULL;
    return 0;
}


void dwarf_inc_ref(void) {
}

void dwarf_dec_ref(void) {
}

StgBool dwarf_try_unload(void) {
    return 0;
}

StgBool dwarf_is_loaded(void) {
    return 0;
}

#endif /* USE_ELF */
