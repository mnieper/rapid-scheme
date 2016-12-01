	.section	rapid_text, "awx", @progbits
	.global	rapid_run
	.set	rapid_run, rapid_text + 0x18
	.balign	16
	.quad	0x98
	.quad	0x0
	.quad	0xfffffffffffffff2
	.quad	0x5c830f0000fc8166
	.quad	0x25848b48000000
	.quad	0x20848b4864000000
	.quad	0xc0814800000008
	.quad	0x830fe03b48000000
	.quad	0x8c481480000003b
	.quad	0xffc6058d48000000
	.quad	0xbe48e7894850ffff
	.quad	0x1
	.quad	0xff000000071d8d48
	.quad	0x8b480000001025a4
	.quad	0x4864000000002584
	.quad	0x480000000020a48b
	.quad	0xbf
	.quad	0x202594ff00
	.quad	0xffffffffffffff72
	.quad	0x0
	.end	
