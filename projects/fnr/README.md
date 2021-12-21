# fnr – find and replace

Like `sed`, but with a memorable interface.

Recursively find and replace patterns in files and directories.

```
fnr [OPTIONS] FIND REPLACE [PATH...]
```

## About

Built on top of [ripgrep]'s path traversal and pattern matching.

**fnr is alpha quality.** Don't use `--write` in situations you
wouldn't be able to revert.

[ripgrep]: https://github.com/BurntSushi/ripgrep

## Examples

``` console
Replace "old_function" with "new_function" in current directory.
$ fnr old_function new_function

Choose files and directories to consider
$ fnr 'EDITOR=vim' 'EDITOR=emacs' ~/.zshrc ~/.config/

We can use --literal so the pattern isn't treated as a regular expression.
$ fnr --literal 'i += 1' 'i++'

Replace using capturing groups.
$ fnr 'const (\w+) = \d+;' 'const $1 = 42;'

Use -w, --write to write changes back to files.
$ fnr --write 'Linus Torvalds' 'Linux Torvalds'

Use -I, --include to only modify files or directories matching a pattern.
$ fnr --include 'Test.*\.kt' 'mockito' 'mockk'

Similarly, use -E, --exclude to ignore certain files.
$ fnr --exclude ChangeLog 2021 2022

Files and directories to consider can also be given over standard input.
$ find /tmp/ -name "*.csv" -print | fnr "," "\t"

Use -p, --prompt to individually accept or reject each replacement.
$ fnr --prompt --literal 'i++' '++i'
--- ./README.md: 2 matching lines
-   18: $ fnr --literal 'i += 1' 'i++'
+   18: $ fnr --literal 'i += 1' '++i'
Stage this replacement [y,n,q,a,e,d,?] ?
```

## Install

```
cargo install fnr
```
