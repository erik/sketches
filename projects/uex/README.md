# uex

Dead simple terminal IRC client.

`uex` simply appends formatted lines to an output file, and handles
input through a named pipe.

```
/path/to/uex/directory
└─── freenode
    ├── #ascii.town
    │   ├── in
    │   └── out
    ├── #lobsters
    │   ├── in
    │   └── out
    ├── #python
    │   └── in
    │   └── out
    └── _server
        ├── in
        └── out
```

Inspired by suckless' [ii], but with less of a focus on being as
minimal as possible (so a single daemon can handle multiple networks,
there is some error handling, etc.).

[ii]: https://tools.suckless.org/ii/

## client

`uex` also includes a simple terminal client which wraps the input /
output in a command. Effectively, it just calls `tail -F
#channel/out` and uses `readline` for input line editing.

``` bash
./client path/to/uex/directory/\#python
```

Client only supports some basic IRC commands.

| Command          | Description                                 |
| -------:         | :-----------                                |
| `/switch BUFFER` | Switch view to `BUFFER`                     |
| `/list`          | Show all active buffers on current network. |
| `/refresh`       | Rerender all history for current buffer.    |
| `/r[econnect]`   | Close and reconnect to current network.     |
| `/j[oin]`        | Join a channel.                             |
| `/quote MSG`     | Send `MSG` to server directly.              |
| `/me ACTION`     | Send CTCP ACTION.                           |

## note

It's probably not worth setting this up. It's very opinionated and
there's no documentation.
