# hem

Like `fmt(1)`, but optimized for code instead of email.

Because `hem` is mostly intended to be used as in Vim and co's `!commands`, it
has to operate with very little context about where the input text is from, and
uses simple heuristics and opinionated defaults to guess at the what the right
wrapping is.

Some examples

```
x = 

in:
    # This comment should spill over to the next line, but we want to keep the leading spaces
out:
    # This comment should spill over to the next line, but we want to keep the
    # leading spaces

in: 
  * we're in the middle of a C++ block comment
  * that wraps over multiple lines
out:
  * we're in the middle of a C++ block comment that wraps over multiple lines
```
    
