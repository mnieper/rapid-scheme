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

#include <librapid.h>

#include <errno.h>
#include <stdbool.h>
#include <stddef.h>
#include <sys/mman.h>

#include <stdio.h>

#include "error.h"

#define HEAP_SIZE 1ULL << 30
#define FIELD_MASK (1ULL << 48) - 1
#define FIELD_TAG_MASK 7 | (~FIELD_MASK)
#define FIELD_LINK 2
#define FIELD_RECORD 4
#define FIELD_FORWARD 6

/* Garbage collection internals
 *
 * Pointers are aligned at 8 byte boundaries.
 * The lowest bit has to be cleared to 0 so that it is not mistaken as an integer.
 * Thus it leaves 2 bits to mark pointers: xy0
 *
 * Bit 000: ordinary pointer
 * Bit 010: pointer to head of module
 * Bit 100: head of vector
 * Bit 110: forwarded vector
 *
 * When object points to another object, scan until head of vector is found.
 * If this is forwarded, adjust pointer.  Otherwise, move whole vector (must move whole module).
 * => no special vector head... => vector just points to head
 */


// we need a heap, which is going to be filled by the gc...
// no need to copy...

static RapidField heap;
static RapidField heap_free;

void
rapid_init_gc (void)
{
  heap = mmap (NULL, HEAP_SIZE, PROT_EXEC | PROT_READ | PROT_WRITE,
	       MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (heap == MAP_FAILED)
    {
      error (1, errno, "could not initialize heap");
    }
  heap_free = heap;
}

static bool
is_special_value (RapidValue value)
{
  return (value | FIELD_TAG_MASK) != 0;
}

static RapidField
find_header(RapidField field)
{
  RapidField p = field;
  do {
    --p;
  } while (!is_special_value (*p));
  switch (*p | FIELD_TAG_MASK)
    {
    case FIELD_LINK:
      return (void *) (*p & ~7);
    case FIELD_RECORD:
      return p;
    case FIELD_FORWARD:
      return p;
    }
}

static
RapidField forward (RapidField header)
{
  return header;
}

static void
process (RapidField *field)
{
  RapidField header = find_header (*field);
  RapidField forwarded_module = forward (header);  // does the copying
  *field = forwarded_module + (header - *field);
}

void
rapid_gc (RapidField roots[], int root_num)
{
  for (int i = 0; i < root_num; ++i)
    {
      //process (&roots[i]);
    }
  // gehe code in elf binary durch
  // gehe code im heap durch (ist durch copying mehr geworden)
  
  // error (1, 0, "heap overflow");
}
