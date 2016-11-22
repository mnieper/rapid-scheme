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

#if HAVE_CONFIG_H
# include <config.h>
#endif
#include <stddef.h>

#include "rapidcommon.h"
#include "macros.h"

static RapidValue stack[10000];
static size_t stack_free = 0;

static void
gc (RapidValue *root);

static RapidField
alloc_record (size_t size);

static RapidValue
cons (RapidValue car, RapidValue cdr);

static RapidValue
car (RapidValue pair);

static RapidValue
cdr (RapidValue pair);

static RapidValue
box (int i);

static int
unbox (RapidValue value);

int
main (int argc, char *argv)
{
  rapid_gc_init (NULL, NULL);

  RapidValue p = cons (box (1),
		       cons (box (2),
			     cons (box (3), box (4))));
  
  gc (&p);

  RapidValue q = cons (box (5), p);

  gc (&q);

  /* clear stack */
  for (int i = 0; i < 10000; ++i)
    stack[i] = 0;

  ASSERT (unbox (car (q)) == 5);
  q = cdr (q);
  ASSERT (unbox (car (q)) == 1);
  q = cdr (q);
  ASSERT (unbox (car (q)) == 2);
  q = cdr (q);
  ASSERT (unbox (car (q)) == 3);
  ASSERT (unbox (cdr (q)) == 4);
}

RapidValue
box (int i)
{
  return (i << 1) | VALUE_TAG_SCALAR;
}

int
unbox (RapidValue value)
{
  return value >> 1;
}

RapidField
alloc_record (size_t size)
{
  stack[stack_free++] = 8 * (size + 1) + VALUE_TAG_RECORD;
  RapidField p = &stack[stack_free];
  stack_free += (size & ~1) + 1;
  return p;
}

RapidValue
cons (RapidValue car, RapidValue cdr)
{
  RapidField p = alloc_record (2);
  p[0] = car;
  p[1] = cdr;
  return (RapidValue) p;
}

RapidValue
car (RapidValue pair)
{
  return ((RapidField) pair)[0];
}		   

RapidValue
cdr (RapidValue pair)
{
  return ((RapidField) pair)[1];
}

void
gc (RapidValue *root)
{
  rapid_gc (root, 1);
  stack_free = 0;
}
