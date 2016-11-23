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

#ifndef GC_UTIL_H_INCLUDED
#define GC_UTIL_H_INCLUDED

#include "rapidcommon.h"

void
gc (RapidValue *root);

RapidField
alloc_record (size_t size);

RapidValue
cons (RapidValue car, RapidValue cdr);

RapidValue
car (RapidValue pair);

RapidValue
cdr (RapidValue pair);

RapidValue
box (int i);

int
unbox (RapidValue value);

#endif /* GC_UTIL_H_INCLUDED */
