\ This is what line comments look like.

( We can also use block comments. They can span
  multiple lines (and be nested like this ))

\ Forth is a stack based language, so everything is pushed and popped from a
\ single shared stack

1 2    \ push 1 to the stack, then push 2
+      \ pop 2 values off the stack, add them, and push the result
.      \ pop top of stack and print it out
cr     \ print a newline

\ Here's how we define a word (effectively a function). It'll print "hello
\ world" when it's invoked.
: hello-world ." hello, world" ;

\ This is a case-insensitive implementation
HELLO-world CR

: control-flow ( -- )
    \ Check if 0 > 5 (you never know!)
    0 5 > IF ." 0 > 5" ELSE ." 0 <= 5" THEN CR

    \ Here's how we loop from 0 to 10 (exclusive)
    10 0 DO I . LOOP CR
;

control-flow

( Here's a tiny quine ) SOURCE TELL CR

\ You can use SOURCE and >IN to manipulate where the interpreter is reading
SOURCE NIP >IN ! -- this part isn't evaluated

\ Pull in definitions from another file. This contains some debugging utilities
\ which are helpful for investigating issues (for example, SEE)
REQUIRE tools.f

\ Disassemble a word so we can see how it's defined.
\
\ SEE tries to return something that could be copy-pasted into a new
\ definition, but it doesn't preserve the exact source code of the word
: add1 1 + ;
SEE add1

\ Try this one to see the raw BRANCH instructions that IF..THEN and DO..LOOP
\ get compiled into.
\ SEE control-flow

\ Here's some magic. A word which pushes the next Fibonacci number
\ every time you call it.
\
\ No I don't expect this to make sense.
: fib-machine
    CREATE
        0 , 1 ,
    DOES>
        DUP @ OVER CELL+ @
        TUCK +
        -ROT OVER !
        OVER SWAP CELL+ !
;

fib-machine foo
fib-machine bar

\ By the way, loops (and if statements and such) can't run at the top level, we
\ need to wrap them in an anonymous function.
:NONAME
    ." foo: " 10 0 DO foo . LOOP CR

    \ What happens to `foo` doesn't impact `bar`
    ." bar: " bar . CR
; EXECUTE

\ quit the process
BYE

\ angrily quit the process (we won't get here though)
-1 DIE
