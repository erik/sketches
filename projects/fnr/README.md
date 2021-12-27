# fnr – find and replace

Like `find ... | xargs sed ...`, but with a memorable interface.

Recursively find and replace patterns in files and directories.

```
fnr [OPTIONS] FIND REPLACE [PATH...]
```

## About

`fnr` is intended to be more intuitive to use than `sed`, but is not a
drop in replacement. `fnr` is instaead focused on making changes in a
directory, and is more comparable with your IDE or editor's find and
replace tool.

Built on top of [ripgrep]'s path traversal and pattern matching, so
even though performance isn't an explicit goal, it's fairly quick.

``` console
$ time fnr -ic rust 'rust!!' target/doc > /dev/null
fnr -ic rust 'rust!!' target/doc > /dev/null  0.29s user 0.22s system 99% cpu 0.511 total

$ time ag -i rust target/doc > /dev/null
ag -i rust target/doc > /dev/null  3.12s user 0.27s system 108% cpu 3.134 total

$ time find target/doc -type f | xargs -I{} sed -i '' 's/rust/rust!!/g' '{}'
find target/doc -type f  0.01s user 0.04s system 0% cpu 13.846 total
xargs -I{} sed -i '' 's/rust/rust!!/g' '{}'  6.40s user 11.41s system 93% cpu 19.044 total
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
