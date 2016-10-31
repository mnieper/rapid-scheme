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
	leaq	.Lrapid_gst(%rip), %rbp
	movq	.Lrapid_lst@gottpoff(%rip), %rax
	movq	%rsp, %fs:.Lheap_start(%rax)
	movq	%rax, .Llocals(%rbp)
	movq	stdout(%rip), %rax
	movq	%rax, .Lstdout(%rbp)
	jmpq	*.Lrapid_run(%rbp)

	.balign	8
.Lrapid_gst:
	.include	"global-symbols.s"

	.section	.tbss, "awT", @nobits
	.balign	8
.Lrapid_lst:
	.include	"local-symbols.s"
