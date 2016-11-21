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

#ifndef MACROS_H_INCLUDED
#define MACROS_H_INCLUDED

#include <stdio.h>
#include <stdlib.h>

#ifndef ASSERT_STREAM
# define ASSERT_STREAM stderr
#endif

#define ASSERT(expr)							\
  do									\
    {									\
      if (!(expr))							\
	{								\
	  fprintf (ASSERT_STREAM, "%s:%d: assertion '%s' failed\n",	\
		   __FILE__, __LINE__, #expr);				\
	  fflush (ASSERT_STREAM);					\
	  abort ();							\
	}								\
    }									\
  while (0)

#endif /* MACROS_H_INCLUDED */
