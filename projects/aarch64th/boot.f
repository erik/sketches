: \ IMMEDIATE
    [ HERE @ ] KEY 10 = 0BRANCH [ HERE @ - , ] ;

\ Now we've defined line comments! We implemented them at a very low level because
\ we don't have language constructs like loops or if statements yet.
\
\ Explaining what happened:
\
\   [ HERE @ ]       \ Push current pos to stack (at compile time)
\   KEY 10 =         \ Read next key of input and check if it's a newline (0x0A)
\   0BRANCH          \ If not, jump back ...
\   [ HERE @ - , ]   \ By however many words we've compiled since start of fn
\
\ Now it'll be easier to build more of the language in an understandable way.
\ Onward!

\
\ Conditional logic
\

\ cond IF true-path THEN
\
\ This is adding some structure to the 0BRANCH approach we did above to define
\ line comments. We don't want to have to count the number of words to jump by
\ hand (it could change!), so we use THEN to rewrite the code compiled by IF
\ based on how far we've moved (tracked by HERE).
\
\ ( cond -- cond offset )
: IF IMMEDIATE
    ' 0BRANCH ,     \ compile 0BRANCH
    HERE @          \ push our current position to the stack
    0 ,             \ compile placeholder jump offset (overwritten later by THEN)
;

\ Finalizer for IF
\
\ ( offset_addr -- )
: THEN IMMEDIATE
    DUP             \ dup previous position set by IF
    HERE @          \ push current position to the stack
    SWAP -          \ how far have we moved? (curr - prev)
    SWAP !          \ *offset_addr = (curr - prev)
;

\ cond IF true-part ELSE false-part THEN
\
\ ( if_offset_addr -- else_offset_addr )
: ELSE IMMEDIATE
    ' BRANCH ,      \ compile a jump to end of ELSE
    HERE @          \ push current position to stack
    0 ,             \ compile placeholder offset for BRANCH (overwritten by THEN)
    SWAP            \ move if_offset_addr to top of stack
    DUP             \ see defintion of THEN above
    HERE @
    SWAP -
    SWAP !
;

\ BEGIN loop-part cond UNTIL
\
\ Loop as long as `cond` evaluates to 0.
\
\ ( -- jmp_offset_addr )
: BEGIN IMMEDIATE
    HERE @          \ push current position to stack
;

\ Finalizer for BEGIN.
\
\ ( jmp_offset_addr cond -- )
: UNTIL IMMEDIATE
    ' 0BRANCH ,    \ compile conditional branch
    HERE @ -       \ how far have we moved from BEGIN?
    ,              \ compile the offset here
;

\ Pop a value off the stack and compile it into `LIT [val]`
\
\ ( a -- )
: LITERAL IMMEDIATE
    ' LIT ,
    ,         \ compile literal from the stack
    ;

\ '[COMPILE] word' compiles a word that would otherwise be executed immediately
\
\ Conceptually similar to `' word ,` if `word` is immediate.
: [COMPILE] IMMEDIATE
	WORD FIND >CFA ,
;

\ We don't have a native concept of characters ('\n' is simply a word called
\ "'\n'"), so let's define a few that we'll need later. Each simply pushes the
\ named character to the stack.
\
\ ( -- a )
: '\n' 10 ;
: BL   32 ;  \ BL(ank) = SPACE
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;

\ Let's implement block comments. Continues until it sees the matching ')', and
\ can be nested to an arbitrary depth, as long as the parens are balanced
: ( IMMEDIATE
    1                              \ nested level
    BEGIN
        KEY
        DUP '(' =
            IF
                DROP 1 +           \ increase depth
            ELSE ')' = IF 1 - THEN \ decrease depth
            THEN
        DUP 0 = UNTIL              \ do we have matched parens?
    DROP
;

( now we have ( nested ) comment syntax! )


\ CR prints a carriage return
: CR '\n' EMIT ;

\ SPACE prints a space
: SPACE BL EMIT ;

\
\ Basic helper utils
\

: cell-size 8 ;
: cell+ ( a -- a+cell ) cell-size + ;
: cell- ( a -- a-cell ) cell-size - ;
: TRUE  ( -- a )     1 ;
: FALSE ( -- a )     0 ;
: 0=    ( a -- b )   0 = ;
: NOT   ( a -- b )   FALSE = ;
: 1+    ( a -- a+1 ) 1 + ;
: 1-    ( a -- a-1 ) 1 - ;

\ BEGIN condition WHILE loop-part REPEAT
\
\ An alternative loop syntax to UNTIL
: WHILE ( jmp_addr cond -- ) IMMEDIATE
    ' NOT ,        \ Invert cond (since 0BRANCH branches when FALSE)
    ' 0BRANCH ,    \ compile conditional branch
    HERE @ -       \ how far have we moved from BEGIN?
    ,              \ compile the offset here
;

\ BEGIN loop-part AGAIN
\
\ Loop forever (or at least until EXIT is called).
: AGAIN ( jmp_addr -- ) IMMEDIATE
    ' BRANCH ,     \ branch unconditionally
    HERE @ - ,     \ to however far we've moved from BEGIN
;

\ cond UNLESS false-part ELSE true-part THEN
\
\ An inverted IF
: UNLESS IMMEDIATE ( cond -- offset_addr )
    ' NOT ,        \ compile NOT (invert cond)
    [COMPILE] IF
;

\ Return the current depth of the stack (in bytes)
: DEPTH ( -- a)
	S0 @ DSP@ -  \ compare current stack pointer with top of stack
	cell-        \ stack pointer points to NEXT word, not current word
;

: test1
    1 IF 1111 . CR THEN
    1 UNLESS 2222 . CR THEN
    0 IF 3333 . CR THEN
    0 UNLESS 4444 . CR THEN
    ;

1 . CR
test1
2 . CR
