# aarch64th

This is a self-hosted 64 bit indirect threaded Forth bootstrapped from AAarch64
assembly for Mac OS.

1. Is it fast? Not particularly.
2. Is it novel? Not very.
3. Should you use it? Definitely not.

That said, there aren't all that many examples of assembly language programs
for Apple Silicon out there, and since this was an educational project for me,
the code is heavily commented in both assembly and Forth. This is probably one
of the gentler introductions to both.

As much as is practical is implemented inside Forth itself from a small set of
words written in assembly, but we don't go anywhere near as far down the rabbit
hole as something like [Planckforth](https://github.com/nineties/planckforth)
(check it out, it's very cool!).

[jonesforth](http://git.annexia.org/?p=jonesforth.git) was a huge help
bootstrapping the assembly runtime, and many of the fundamental design decisions
(e.g. the indirect threaded model) come from there.

It implements most core functionality from
["standard"](https://forth-standard.org) Forth.
