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

#include <bfd.h>
#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

#include "rapidcommon.h"
#include "error.h"

#define HEAP_SIZE 1ULL << 30

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

static void
process_field (RapidValue *value);

static void
process_module (RapidField module);

static RapidField heap;
static RapidField heap_free;
static RapidField text_start;
static RapidField text_end;

void
rapid_gc_init (RapidField start, RapidField end)
{
  heap = mmap (NULL, HEAP_SIZE, PROT_EXEC | PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (heap == MAP_FAILED)
    {
      error (1, errno, "could not initialize heap");
    }
  heap_free = heap;

  text_start = start;
  text_end = end;

  bfd_init ();
  bfd_set_error_program_name (program_invocation_name);
}

void
rapid_gc (RapidValue roots[], int root_num)
{
  for (int i = 0; i < root_num; ++i)
    {
      process_field (&roots[i]);
    }
  for (RapidField module = text_start; module < text_end; module += get_module_size (module))
    {
      process_module (module);
    }
  for (RapidField module = heap; module < heap_free; module += get_module_size (module))
    {
      process_module (module);
    }
}

void
rapid_gc_dump (const char *filename, RapidValue entry)
{
  bfd *abfd = bfd_openw (filename, "elf64-x86-64");
  if (abfd == NULL)
    {
      bfd_perror ("cannot open object file for writing");
      exit (1);
    }
  
  if (!bfd_set_format (abfd, bfd_object))
    {
      bfd_perror ("cannot set format of object file");
      exit (1);
    }
  
  asection *section = bfd_make_section_with_flags (abfd, "rapid_text",
						   SEC_ALLOC | SEC_CODE | SEC_RELOC |
						   SEC_HAS_CONTENTS);
  if (section == NULL)
    {
      bfd_perror ("cannot create text section");
      exit (1);
    }
  section->alignment_power = 4;

  // TODO: compactify into extra buffer; change entry as needed!
  size_t size = 0; /* DO SOME COMPACTING STARTING WITH ENTRY; THIS SHOULD SET SIZE */
  
  if (!bfd_set_section_size (abfd, section, size))
    {
      bfd_perror ("cannot set section size");
      exit (1);
    }

  // TODO: Write rapid_run (based on entry) as entry point
  asymbol *symbol = bfd_make_empty_symbol (abfd);
  asymbol *ptrs[3];
  symbol->name = "rapid_text_start";
  symbol->section = section;
  symbol->flags = BSF_GLOBAL;
  symbol->value = 0;
  ptrs[0] = symbol;
  symbol = bfd_make_empty_symbol (abfd);
  symbol->name = "rapid_text_end";
  symbol->section = section;
  symbol->flags = BSF_GLOBAL;
  symbol->value = size;
  ptrs[1] = symbol;
  ptrs[2] = 0;
  if (!bfd_set_symtab (abfd, ptrs, 2))
    {
      bfd_perror ("cannot set symbols");
      exit (1);
    }
  
  /* write contents 
  if (!bfd_set_section_contents (abfd, section, contents, 0, size))
    {
      bfd_perror ("cannot write section");
      exit (1);
    }
  */
  
  if (!bfd_close (abfd))
    {
      bfd_perror ("finishing writing object file failed");
      exit (1);
    }
}

bool
is_scalar_value (RapidValue value)
{
  return (value & VALUE_TAG_SCALAR) || (value << 16 >> 16 != value);
}

int
get_value_tag (RapidValue value)
{
  return (is_scalar_value (value)) ? VALUE_TAG_NONE : value & VALUE_TAG_MASK;
}

RapidField
value_to_pointer (RapidValue value)
{
  return (RapidField) (value | ~VALUE_TAG_MASK);
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
  } while (0);
}

size_t
get_module_size (RapidField field)
{
  return ((((uintptr_t) *field) >> 3) + 1) & ~1;
}

RapidField
forward_module (RapidField header)
{
  if (header >= text_start && header < text_end)
    return header;
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
  size_t var_num;
  RapidField p;

  if (get_value_tag (*module) == VALUE_TAG_RECORD)
    {
      p = module + 1;
      var_num = record_to_num (*module) - 1;
    }
  else
    {
      var_num = module[1] >> 3;
      p = module + (module[0] >> 3 - var_num);
    }

  for (size_t i = 0; i < var_num; ++i)
    {
      process_field (&p[i]);
    }
}
