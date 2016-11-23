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

#ifndef RAPIDCOMMON_H_INCLUDED
#define RAPIDCOMMON_H_INCLUDED

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define VALUE_TAG_MASK    7
#define VALUE_TAG_NONE    0
#define VALUE_TAG_SCALAR  1
#define VALUE_TAG_LINK    2
#define VALUE_TAG_RECORD  4
#define VALUE_TAG_FORWARD 6

typedef intptr_t RapidValue;
typedef RapidValue *RapidField;

void
rapid_gc_init (RapidField text_start, RapidField text_end);

void
rapid_gc (RapidValue roots[], int root_num);

bool
rapid_gc_dump (RapidValue roots[], int root_num, const char *filename, RapidField entry);

#endif /* LIBCOMMON_H_INCLUDED */