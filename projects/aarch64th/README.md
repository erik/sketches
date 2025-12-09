# aarch64th

Self-hosted 64 bit indirect threaded Forth bootstrapped from AAarch64 assembly
for MacOS.

1. Is it fast? No.
2. Is it useful? No.

As much as is practicle is implemented inside Forth itself from a small set of
words written in assembly, but we don't go anywhere near as far down the rabbit
hole as something like [Planckforth](https://github.com/nineties/planckforth).

[jonesforth](http://git.annexia.org/?p=jonesforth.git) was a huge help
bootstrapping the assembly runtime, and many of the fundamental design decisions
(e.g. the indirect threaded model) come from there.

It implements most core functionality from
["standard"](https://forth-standard.org) Forth.
