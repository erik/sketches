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
    ' LIT , ,
;

\ '[COMPILE] word' compiles a word that would otherwise be executed immediately
\
\ Conceptually similar to `' word ,` if `word` is immediate.
: [COMPILE] IMMEDIATE
	WORD FIND >CFA ,
;

\ "compile time tick". Leaves execution token (addr) of word on the stack
\
\ ( -- xt )
: ['] IMMEDIATE
    ' LIT ,
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

\ Compile-time CHAR
: [CHAR] IMMEDIATE
    CHAR
    [COMPILE] LITERAL
;

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

: cell-size CELL ;
: cell+ ( a -- a+cell ) CELL + ;
: cell- ( a -- a-cell ) CELL - ;
: CELLS ( a -- a*cell-size) CELL * ;
: TRUE  ( -- a )     1 ;
: FALSE ( -- a )     0 ;
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

\ Push to return stack from data stack.
: >R ( a -- R:a )
    RP@          \ get current rstack ptr         (a -- a &rp)
    RP@ @        \ get current return addr        (a &rp -- a &rp *rp)
    RP@ cell-    \ get next rstack ptr            (a &rp *rp -- a &rp *rp &rp-1)
    DUP RP!      \ set rstack ptr to next
    !            \ store cur return addr in       (a &rp *rp &rp-1 -- a &rp)
                 \ next position (so it's on top)
    !            \ store `a` in *rsp              (a &rp -- )
;

\ Push to data stack from return stack
: R> ( R:a -- a )
    RP@ cell+ @  \ get data before current return addr   ( -- a )
    RP@ @        \ get current return addr               ( a -- a *rp )
    RP@ cell+    \ get addr below top of stack           ( a *rp -- a *rp &rp-1 )
    DUP RP!      \ "pop" rstack ptr
    !            \ move return addr to new top of stack  ( a *rp &rp-1 -- a )
;

: NIP   ( a b -- b )          SWAP DROP ;
: TUCK  ( a b -- b a b )      SWAP OVER ;
: 2DUP  ( a b -- a b a b )    OVER OVER ;
: 2DROP ( a b -- )            DROP DROP ;
: 3DROP ( a b c -- )          2DROP DROP ;
: ROT   ( a b c -- b c a )    >R SWAP R> SWAP ;
: -ROT  ( a b c -- c a b )    SWAP >R SWAP R> ;
: PICK  ( x0 ... xn i -- x0 ... xi x0 )
    CELLS
    SP@ +
    @
;
: >  ( a b -- c ) 2DUP = NOT IF < NOT ELSE 2DROP FALSE THEN ;
: <= ( a b -- c ) 2DUP = IF 2DROP TRUE ELSE < THEN ;
: >= ( a b -- c ) 2DUP = IF 2DROP TRUE ELSE > THEN ;
: 0=    ( a -- b )   0 = ;
: 0<    ( a -- b )   0 < ;
: 0>    ( a -- b )   0 > ;
: 0<=   ( a -- b )   0 <= ;
: 0>=   ( a -- b )   0 >= ;

: ?DUP  ( 0 -- 0 | x -- x x ) DUP DUP 0= IF DROP THEN ;

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

\ Return the number of elements in the stack
: DEPTH     SP0 SP@ - cell- cell-size / ;
: RDEPTH    RP0 RP@ - cell-size / ;

\ Align input to cell size (e.g. 3 -> 8, 15 -> 16)
: ALIGNED ( a -- a )
    cell-size 1- +
    cell-size 1- INVERT
    AND
;

: ?ALIGNED ( a -- a )
    cell-size 1-
    AND
    0=
;

\ Update HERE to be cell aligned
: ALIGN  ( -- )
    HERE @ ALIGNED
    HERE !
;

: ?immediate ( -- a )
    STATE @ 0= ;
: ?compiling ( -- a )
    ?immediate NOT ;

: S" IMMEDIATE ( -- addr len )
    ?compiling IF
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

\ Create and immediately print a literal string.
: ." IMMEDIATE ( -- )
    ?compiling IF
        [COMPILE] S"  \ read + compile the literal string
        ' TELL ,      \ compile word to print it
    ELSE              \ we're in immediate mode
        BEGIN         \ loop through each char of string until closing quote
            KEY
            DUP '"' !=
        WHILE
            EMIT     \ print each byte
        REPEAT
        DROP         \ drop closing quote
    THEN
;

\ val CONSTANT name
\
\ Define a word (immediate value) which pushes a constant value to the stack
: CONSTANT ( val -- )
    CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
;

: ALLOT ( n -- ) HERE +! ;

\ Allocate a cell of memory as a named variable
\
\ Define: VARIABLE foo
\ Read  : foo @
\ Write : 1 foo !
: VARIABLE
    HERE @
    cell-size ALLOT
    CREATE
    DOCOL ,
    ' LIT ,
    ,
    ' EXIT ,
;

\ val >= lo && val < hi
: WITHIN ( val lo hi -- a )
    3 PICK >       \ ( val lo hi val --> val lo <hi )
    -ROT >=        \ ( <hi val lo    --> <hi >=lo)
    AND
;

: STATE-ON  ( -- )  1 STATE ! ;
: STATE-OFF ( -- )  0 STATE ! ;

: EXECUTE-COMPILING ( i*x xt --j*x )
    STATE @ IF
        EXECUTE
        EXIT
    ELSE
        STATE-ON
        EXECUTE
        STATE-OFF
    THEN
;

\ Compile a reference to the word currently being defined.
: RECURSE IMMEDIATE ( -- )
    LATEST @ >CFA ,
;


VARIABLE HANDLER 0 HANDLER !     \ last exception handler

\ TODO: document me
\
\ CATCH and THROW are from the implementation given in
\ https://forth-standard.org/standard/exception/THROW
: CATCH ( xt -- exception# | 0 )
   SP@ >R                ( xt )       \ save data stack pointer
   HANDLER @ >R          ( xt )       \ and previous handler
   RP@ HANDLER !         ( xt )       \ set current handler
   EXECUTE               ( )          \ execute returns if no THROW
   R> HANDLER !          ( )          \ restore previous handler
   R> DROP               ( )          \ discard saved stack ptr
   0                     ( 0 )        \ normal completion
;

: THROW ( ??? exception# -- ??? exception# )
    ?DUP IF	             ( exc# )     \ 0 THROW is no-op
      HANDLER @ RP!      ( exc# )     \ restore prev return stack
      R> HANDLER !	     ( exc# )     \ restore prev handler
      R> SWAP >R	     ( saved-sp ) \ exc# on return stack
      SP! DROP R>	     ( exc# )     \ restore stack

      \ Return to the caller of CATCH because return
      \ stack is restored to the state that existed
      \ when CATCH began execution
    THEN
;
