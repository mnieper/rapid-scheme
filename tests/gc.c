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

#include "gc_util.h"
#include "rapidcommon.h"
#include "macros.h"


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

  ASSERT (unbox (car (q)) == 5);
  q = cdr (q);
  ASSERT (unbox (car (q)) == 1);
  q = cdr (q);
  ASSERT (unbox (car (q)) == 2);
  q = cdr (q);
  ASSERT (unbox (car (q)) == 3);
  ASSERT (unbox (cdr (q)) == 4);
}
