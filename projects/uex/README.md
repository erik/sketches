# uex

Dead simple terminal IRC client.

`uex` simply appends formatted lines to an output file, and handles
input through a named pipe.

```
/path/to/uex/directory
├── freenode
│   ├── #ascii.town
│   │   ├── __in
│   │   └── __out
│   ├── #lobsters
│   │   ├── __in
│   │   └── __out
│   ├── #python
│   │   └── __in
│   │   └── __out
│   └── _server
│       ├── __in
│       └── __out
```

Inspired by suckless' [ii], but with less of a focus on being as
minimal as possible (so a single daemon can handle multiple networks,
there are retries and some error handling, etc.).

[ii]: https://tools.suckless.org/ii/

`uex` also includes a simple terminal client which wraps the input /
output in a command. Effectively, it just calls `tail -F
#channel/__out` and uses `readline` for input line editing.

``` bash
./client path/to/uex/directory/\#python
```
