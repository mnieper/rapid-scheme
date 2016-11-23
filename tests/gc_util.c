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

#include "gc_util.h"

static RapidValue stack[10000];
static size_t stack_free = 0;


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
