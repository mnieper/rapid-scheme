# Copyright (C) 2016  Marc Nieper-Wißkirchen

# This file is part of Rapid Scheme.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

	.section	rapid_text, "awx"
	.global	main
	.type	main, @function
main:
	pushq	%rbp
	leaq	Lrapid_gpt(%rip), %rbp
	leaq	str(%rip), %rdi
	call	*8(%rbp)
	jmpq	*(%rbp)
	.balign	8
Lrapid_gpt:
	.quad	rapid_run
	.quad	puts
str:	.asciz	"Hello, World!"
