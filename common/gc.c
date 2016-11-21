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

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <sys/mman.h>

#include <stdio.h>

#include "rapidcommon.h"
#include "error.h"

#define HEAP_SIZE 1ULL << 30

static int
get_value_tag (RapidValue);

static RapidField
value_to_pointer (RapidValue value);

static size_t
value_to_offset (RapidValue value);

static RapidField
get_module_header (RapidField field);

static size_t
get_module_size (RapidField field);

static RapidField
forward_module (RapidField header);

static void
process_field (RapidField *field);

static void
process_module (RapidField module);

static RapidField heap;
static RapidField heap_free;

void
rapid_gc_init (void)
{
  heap = mmap (NULL, HEAP_SIZE, PROT_EXEC | PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (heap == MAP_FAILED)
    {
      error (1, errno, "could not initialize heap");
    }
  heap_free = heap;
}

void
rapid_gc (RapidField roots[], int root_num)
{
  for (int i = 0; i < root_num; ++i)
    {
      process_field (&roots[i]);
    }
  /* PROCESS CODE IN BINARY (need extra area; give this to rapid_gc_init) */
  for (RapidField module = heap; module < heap_free; module += get_module_size (module))
    {
      process_module (module);
    }
}

int
get_value_tag (RapidValue value)
{
  if (value << 16 >> 16 != value)
    return VALUE_TAG_NONE;
  return value | VALUE_TAG_MASK;
}

RapidField
value_to_pointer (RapidValue value)
{
  return (RapidField) (value | ~VALUE_TAG_MASK);
}

size_t
value_to_offset (RapidValue value)
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
	return field + value_to_offset (*field);
      default:
	return field;
      }
  } while (0);
}

size_t
get_module_size (RapidField field)
{
  return *field >> 3;
}

RapidField
forward_module (RapidField header)
{
  /* FIXME */
  return header;
}

void
process_field (RapidField *field)
{
  RapidField header = get_module_header (*field);
  RapidField forwarded_module = forward_module (header);
  *field = *field + (forwarded_module - header);
}

void
process_module (RapidField module)
{
  /* TODO: Run through all ptrs inside module! */
}
