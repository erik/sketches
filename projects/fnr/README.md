# fnr – find and replace

Like `find ... | xargs sed ...`, but with a memorable interface.

Recursively find and replace patterns in files and directories.

```
fnr [OPTIONS] FIND REPLACE [PATH...]
```

## About

`fnr` is intended to be more intuitive to use than `sed`, but is not a
drop in replacement. `fnr` is instead focused on making changes in a
directory, and is more comparable with your IDE or editor's find and
replace tool.

Built on top of [ripgrep]'s path traversal and pattern matching, so
even though performance isn't an explicit goal, it's fast enough to
not be the bottleneck.

``` console
$ time fnr -iq linux 'gnu/linux' /tmp/linux > /dev/null
fnr -iq linux 'gnu/linux' /tmp/linux > /dev/null  1.73s user 2.12s system 226% cpu 1.701 total

$ time rg -i linux /tmp/linux > /dev/null
rg -i linux /tmp/linux > /dev/null  1.34s user 3.57s system 1130% cpu 0.434 total

$ time ag -i linux /tmp/linux > /dev/null
ag -i linux /tmp/linux > /dev/null  3.58s user 3.18s system 312% cpu 2.161 total

$ time grep -irI linux /tmp/linux > /dev/null
grep -irI linux /tmp/linux > /dev/null  26.09s user 2.22s system 99% cpu 28.327 total

$ time find /tmp/linux -type f | xargs sed -i '' 's;linux;gnu/linux;g'
find /tmp/linux -type f  0.04s user 0.34s system 0% cpu 39.504 total
xargs sed -i '' 's;linux;gnu/linux;g'  19.18s user 22.71s system 97% cpu 42.992 total
```

**fnr is alpha quality.** Don't use `--write` in situations you
wouldn't be able to revert.

[ripgrep]: https://github.com/BurntSushi/ripgrep

## Examples

Replace `"old_function"` with `"new_function"` in current directory.
```
fnr old_function new_function
```

Choose files and directories to consider.
```
fnr 'EDITOR=vim' 'EDITOR=emacs' ~/.zshrc ~/.config/
```

We can use `--literal` so the pattern isn't treated as a regular expression.
```
fnr --literal 'i += 1' 'i++'
```

Replace using capturing groups.
```
fnr 'const (\w+) = \d+;' 'const $1 = 42;'
```

Use `-w --write` to write changes back to files.
```
fnr --write 'Linus Torvalds' 'Linux Torvalds'
```

Use `-I --include` to only modify files or directories matching a pattern.
```
fnr --include 'Test.*\.kt' 'mockito' 'mockk'
```

Similarly, use `-E --exclude` to ignore certain files.
```
fnr --exclude ChangeLog 2021 2022
```

Files and directories to consider can also be given over standard input.
```
find /tmp/ -name "*.csv" -print | fnr "," "\t"
```

Use `-p --prompt` to individually accept or reject each replacement.
```
fnr --prompt --literal 'i++' '++i'
--- ./README.md: 2 matching lines
-   18: $ fnr --literal 'i += 1' 'i++'
+   18: $ fnr --literal 'i += 1' '++i'
Stage this replacement [y,n,q,a,e,d,?] ?
```

## Installation

```
cargo install fnr
```

If you'd prefer to build from source instead:

``` console
$ git clone git@github.com/erik/fnr.git
$ cd fnr
$ cargo install --path .
```

## Similar Tools

If `fnr` doesn't quite fit what you're looking for, also consider:

- [facebookincubator/fastmod](https://github.com/facebookincubator/fastmod/) - quite similar to `fnr`.
- [google/rerast](https://github.com/google/rerast) - operates on Rust AST
- [coccinelle](https://coccinelle.gitlabpages.inria.fr/website/) - more advanced edits to C code
- ... many more
