# Find and Replace

``` console
# Basic string replacement
$ fnr 'search term' 'replacement term' file1 dir1...

Replaced 15 occurrences in 5 files.

# Regex replacement with capturing group
$ fnr '([a-z])([A-Z])' '$2,$1'

Replaced 15 occurrences in 5 files.

# Interactive mode, prompt for each replacement
$ fnr -p 'search' 'replace' file...
path/to/file: 15 matches

  - 78: Here is a line with _search_ term.
  + 78: Here is a line with _replace_ term.

<Y,n,a,d,q,?> ?

y: Replace this instance.
n: Leave this instance intact, do not replace.
a: Replace this instance and all others in this file.
d: Do not replace this instance or any others in this file.
q: Do not replace this instance and terminate the program
?: Display this help text

<Y,n,a,d,q,?> q

Done. Replaced 0 instances

# Replace matches in files given from stdin
$ fd tests/ | fnr 'find' 'replace'

tests/foo: 20 matching lines
tests/bar: 1 matching line
```
