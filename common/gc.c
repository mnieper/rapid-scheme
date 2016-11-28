/*
 * Copyright (C) 2016  Marc Nieper-Wißkirchen
 *
 * This file is part of Rapid Scheme.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

#define _GNU_SOURCE

#include <assert.h>
#include <bfd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "rapidcommon.h"
#include "error.h"
#include "obstack.h"
#include "xalloc.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

#define HEAP_SIZE 1ULL << 30
#define THRESHOLD 1ULL << 20


// TODO: Remove RapidField.  View heap as large array of RapidValue's.
typedef RapidValue *RapidField;

static void
heapify (RapidValue roots[], int root_num);

static bool
is_scalar_value (RapidValue value);

static int
get_value_tag (RapidValue value);

static RapidField
value_to_pointer (RapidValue value);

static ptrdiff_t
link_to_offset (RapidValue value);

static size_t
record_to_num (RapidValue value);

static RapidField
get_module_header (RapidField field);

static size_t
get_module_size (RapidField field);

static RapidField
forward_module (RapidField header);

static size_t
get_var_count (RapidValue *module, RapidValue **vars);

static void
process_field (RapidValue *value);

static void
process_module (RapidField module);

static void
init_heap (void);

static void
free_heap (RapidField heap);

static RapidValue *
switch_heap (void);

static size_t
get_free_space ();

static void
write_object_file (RapidValue *start, RapidValue *end, const char *filename, RapidValue *entry);

static void
clear_section_quad (bfd *abfd, asection *section, file_ptr offset);

static RapidField heap;
static RapidField heap_free;

void
rapid_gc_init ()
{
  init_heap ();
 
  bfd_init ();
  bfd_set_error_program_name (program_invocation_name);
}

void
rapid_gc (RapidValue roots[], int root_num)
{
  RapidValue *old_heap = (get_free_space () < THRESHOLD) ? switch_heap () : NULL;
  heapify (roots, root_num);
  if (old_heap != NULL)
    free_heap (old_heap);
}

bool
rapid_gc_dump (RapidValue roots[], int root_num, const char *filename, RapidField entry)
{
  RapidValue *old_heap = switch_heap ();
  heapify ((RapidValue *) &entry, 1);
  RapidValue *end = heap_free;

  heapify (roots, root_num);
  free_heap (old_heap);

  write_object_file (heap, end, filename, entry);
}

void
heapify (RapidValue roots[], int root_num)
{  
  for (int i = 0; i < root_num; ++i)
    {
      process_field (&roots[i]);
    }
  for (RapidValue *module = heap; module < heap_free; module += get_module_size (module))
    {
      process_module (module);
    }
}

bool
is_scalar_value (RapidValue value)
{
  return (value == 0) || (value & VALUE_TAG_SCALAR) || (value << 16 >> 16 != value);
}

int
get_value_tag (RapidValue value)
{
  return (is_scalar_value (value)) ? VALUE_TAG_NONE : value & VALUE_TAG_MASK;
}

RapidField
value_to_pointer (RapidValue value)
{
  return (RapidField) (value & ~VALUE_TAG_MASK);
}

ptrdiff_t
link_to_offset (RapidValue value)
{
  return value >> 3;
}

size_t
record_to_num (RapidValue value)
{
  return value >> 3;
}

RapidField
get_module_header (RapidField field)
{
  do {
    switch (get_value_tag (*--field))
      {
      case VALUE_TAG_NONE:
	continue;
      case VALUE_TAG_LINK:
	return field + link_to_offset (*field);
      default:
	return field;
      }
  } while (1);
}

size_t
get_module_size (RapidField field)
{
  return ((((uintptr_t) *field) >> 3) + 1) & ~1;
}

RapidField
forward_module (RapidField header)
{
  if (header >= heap && header < heap_free)
    return header;
  
  if (get_value_tag (*header) == VALUE_TAG_FORWARD)
    return value_to_pointer (*header);

  size_t size = get_module_size (header);
  memcpy (heap_free, header, size * 8);
  header[0] = ((RapidValue) heap_free) | VALUE_TAG_FORWARD;
  header = heap_free;
  heap_free += size;

  return header;
}

size_t
get_var_count (RapidValue *module, RapidValue **vars)
{
  if (get_value_tag (*module) == VALUE_TAG_RECORD)
    {
      *vars = module + 1;
      return record_to_num (*module) - 1;
    }
  size_t var_count = module[1] >> 3;
  *vars = module + ((module[0] >> 3) - var_count);
  return var_count;
}

void
process_field (RapidValue *value)
{
  if (is_scalar_value (*value))
    return;
  
  RapidField header = get_module_header ((RapidField) *value);
  RapidField forwarded_module = forward_module (header);
  *(RapidField *) value += (forwarded_module - header);
}

void
process_module (RapidField module)
{
  RapidValue *vars;
  size_t var_num = get_var_count (module, &vars);

  for (size_t i = 0; i < var_num; ++i)
    {
      process_field (&vars[i]);
    }
}

void
init_heap (void)
{
  heap = mmap (NULL, HEAP_SIZE, PROT_EXEC | PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (heap == MAP_FAILED)
    error (1, errno, "could not initialize heap");
  heap_free = heap;
}

RapidValue *
switch_heap (void)
{
  RapidValue *old_heap = heap;
  init_heap ();
  return old_heap;
}

void
free_heap (RapidField heap)
{
  munmap (heap, HEAP_SIZE);
}

size_t
get_free_space ()
{
  return HEAP_SIZE - ((RapidValue) heap_free - (RapidValue) heap);
}

void
write_object_file (RapidValue *start, RapidValue *end, const char *filename, RapidValue *entry)
{
  size_t size = (end - start) << 3;
  struct obstack reloc_stack = {};
  bfd *abfd;
  asection *section;
  asymbol *symbols[2];
  
  obstack_init (&reloc_stack);

  assert ((abfd = bfd_openw (filename, "elf64-x86-64")) != NULL);
  assert (bfd_set_format (abfd, bfd_object));
  assert (bfd_set_arch_mach (abfd, bfd_arch_i386, bfd_mach_x86_64));
  assert ((section = bfd_make_section_with_flags (abfd, "rapid_text",
						 SEC_ALLOC | SEC_CODE | SEC_RELOC |
						  SEC_HAS_CONTENTS)) != NULL);
  section->alignment_power = 4;

  symbols[0] = bfd_make_empty_symbol (abfd);
  symbols[0]->name = "rapid_run";
  symbols[0]->section = section;
  symbols[0]->flags = BSF_GLOBAL;
  symbols[0]->value = (entry - heap) << 3;
  symbols[1] = (asymbol *) NULL;
  assert (bfd_set_symtab (abfd, symbols, 1));
  assert (bfd_set_section_size (abfd, section, size));
  assert (bfd_set_section_contents (abfd, section, heap, 0, size));
 
  unsigned int reloc_count = 0;
  asymbol *section_symbol = bfd_make_empty_symbol (abfd);
  section_symbol->section = section;
  section_symbol->flags = BSF_SECTION_SYM;

  for (RapidValue *module = start; module < end; module += get_module_size (module))
    {
      RapidValue *vars;
      size_t var_count = get_var_count (module, &vars);

      for (size_t i = 0; i < var_count; ++i)
	{
	  RapidField q = vars + i;
	  RapidValue v = *q;
	  if (is_scalar_value (v))
	    continue;

	  reloc_count++;
	  arelent relent = {
	    .sym_ptr_ptr = &section_symbol,
	    .address = (q - heap) * sizeof (RapidValue),
	    .addend = (((RapidField) v) - heap) * sizeof (RapidValue),
	    .howto = bfd_reloc_type_lookup (abfd, BFD_RELOC_64)
	  };
	  
	  obstack_grow (&reloc_stack, &relent, sizeof (arelent));
	  clear_section_quad (abfd, section, (q - heap) << 3);
	}
    }

  arelent *relents = obstack_finish (&reloc_stack);

  for (int i = 0; i < reloc_count; ++i)
    {
      obstack_ptr_grow (&reloc_stack, &relents[i]);
    }
  
  bfd_set_reloc (abfd, section, obstack_finish (&reloc_stack), reloc_count);
  assert (bfd_close (abfd));

  obstack_free (&reloc_stack, NULL);  
}

void
clear_section_quad (bfd *abfd, asection *section, file_ptr offset)
{
  static RapidValue zero = 0;
  assert (bfd_set_section_contents (abfd, section, &zero, offset, sizeof (RapidValue)));
}
