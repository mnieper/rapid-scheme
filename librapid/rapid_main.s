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

	.set	.Lheap_size, 0x1000000

	.section	rapid_text, "awx"
	.global	main
	.type	main, @function
main:
	pushq	%rbp		# Align stack to 16 bytes.
	leaq	.Lrapid_gst(%rip), %rbp # Load absolute address of array of globals
	movq	.Lrapid_lst@gottpoff(%rip), %rbx # Load absolute address of array of thread-locals
	movq	%rsp, %fs:.Lheap_start(%rbx)	 # Store heap start
	leaq	-.Lheap_size(%rsp),%rax		 # Calculate end of heap
	movq	%rax, %fs:.Lheap_end(%rbx)	 # Store heap end
	movq	%rbx, .Llocals(%rbp)		 # Store address of thread locals
	movq	stdout(%rip), %rax		 # Load pointer to stdout
	movq	%rax, .Lstdout(%rbp)		 # Store pointer as global
	jmpq	*.Lrapid_run(%rbp)		 # Jump into trampoline

	## Global variables that are accessed relative to %rpb
	.balign	8
.Lrapid_gst:
	.include	"global-symbols.s"

	## Thread local variables
	.section	.tbss, "awT", @nobits
	.balign	8
.Lrapid_lst:
	.include	"local-symbols.s"
