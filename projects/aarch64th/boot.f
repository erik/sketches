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
: '"' [ CHAR " ] LITERAL ;
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

\ BEGIN loop-part AGAIN
\
\ Loop forever (or at least until EXIT is called).
: AGAIN ( jmp_addr -- ) IMMEDIATE
    ' BRANCH ,     \ branch unconditionally
    HERE @ - ,     \ to however far we've moved from BEGIN
;

\ BEGIN cond WHILE loop-part REPEAT
: WHILE IMMEDIATE ( cond -- )
    ' 0BRANCH ,   \ compile a conditional branch
    HERE @        \ push current location to stack
    0 ,           \ compile a placeholder branch offset (set by REPEAT)
;

\ BEGIN cond WHILE loop-part REPEAT
: REPEAT IMMEDIATE
    ' BRANCH ,    \ compile a conditional branch
    SWAP          \ ( begin_off while_off -- while_off begin_off )
    HERE @ - ,    \ compile how far we've moved since BEGIN
    DUP           \ ( w b -- w b b )
    HERE @        \ ( w b b -- w b b here )
    SWAP -        \ ( w b b h -- w b h b -- w b diff )
    SWAP !        \ ( w b d -- w d b ) -- write diff to begin offset
;

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
: !=    ( a b -- c ) = NOT ;
: 1+    ( a -- a+1 ) 1 + ;
: 1-    ( a -- a-1 ) 1 - ;

: +! ( val addr -- )
    SWAP   ( addr val )
    OVER   ( addr val addr )
    @      ( addr val *addr )
    +      ( addr nval )
    SWAP ! ( val addr )
;
: -! ( val addr -- ) SWAP OVER @ - SWAP ! ;

\
\ String utils
\

\ CR prints a carriage return
: CR '\n' EMIT ;

\ SPACE prints a space
: SPACE BL EMIT ;

\ Compile single character to HERE. Won't leave an aligned pointer!
: C, ( a -- )
    HERE @ C!     \ write a character to *HERE
    1 HERE +!     \ *HERE++
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

\ Align input to cell size (e.g. 3 -> 8, 15 -> 16)
: ALIGNED ( a -- a )
    cell-size 1- +
    cell-size 1- INVERT
    AND
;

\ Update HERE to be cell aligned
: ALIGN  ( -- )
    HERE @ ALIGNED
    HERE !
;

: is-immediate? ( -- a )
    STATE @ 0= ;
: is-compiling? ( -- a )
    is-immediate? NOT ;

: S" IMMEDIATE ( -- addr len )
    is-compiling? IF
        ' LITSTRING ,  \ compile LITSTRING
        HERE @         \ push current position to stack
        0 ,            \ placeholder string length

        BEGIN          \ compile byte by byte until we reach end of string
            KEY
            DUP '"' !=
        WHILE
            C,
        REPEAT
        DROP           \ drop final quote char
        DUP            \ ( len_addr len_addr )
        HERE @ SWAP -  \ push length of string
        cell-          \ placeholder one cell before start of string
        SWAP !         \ write length back to original addr

        ALIGN          \ keep HERE aligned

    ELSE               \ immediate mode
        HERE @         \ push current addr

        BEGIN          \ write byte by byte until end of string
            KEY
            DUP '"' !=
        WHILE
            OVER C!
            1+
        REPEAT
        DROP           \ ignore final quote
        HERE @ -       \ calculate length
        HERE @         \ push start addr (doesn't change because of immediate mode)
        SWAP           \ ( addr len )
    THEN
;
