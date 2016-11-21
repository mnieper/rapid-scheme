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

#ifndef LIBRAPID_H_INCLUDED
#define LIBRAPID_H_INCLUDED

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdint.h>

typedef uintptr_t RapidValue;
typedef RapidValue *RapidField;

void
rapid_gc_init (void);

void
rapid_gc (RapidField roots[], int root_num);

#endif /* LIBRAPID_H_INCLUDED */
