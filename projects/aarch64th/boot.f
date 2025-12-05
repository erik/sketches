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

\ Pop a value off the stack and compile it into `LIT [val]`
\
\ ( a -- )
: LITERAL IMMEDIATE
    ' LIT , ,
;

\ We don't have a native concept of characters ('\n' is simply a word called
\ "'\n'"), so let's define a few that we'll need later. Each simply pushes the
\ named character to the stack.
\
\ ( -- a )
: '\n' 10 ;
: BL   32 ; \ space
: '"' [ CHAR " ] LITERAL ;
: '(' [ CHAR ( ] LITERAL ;
: ')' [ CHAR ) ] LITERAL ;

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

\ '[COMPILE] word' compiles a word that would otherwise be executed immediately
\
\ Conceptually similar to `' word ,` if `word` is immediate.
: [COMPILE] IMMEDIATE
    BL WORD
    FIND
    >CFA ,
;

\ "compile time tick". Leaves execution token (addr) of word on the stack
\
\ ( -- xt )
: ['] IMMEDIATE
    ' LIT ,
;

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


: NOP ;
: CELL+ ( a -- a+CELL ) CELL + ;
: CELL- ( a -- a-CELL ) CELL - ;
: CELLS ( a -- a*CELL) CELL * ;
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
: -! ( val addr -- ) SWAP OVER @ SWAP - SWAP ! ;

\ Push to return stack from data stack.
: >R ( a -- R:a )
    RP@          \ get current rstack ptr         (a -- a &rp)
    RP@ @        \ get current return addr        (a &rp -- a &rp *rp)
    RP@ CELL-    \ get next rstack ptr            (a &rp *rp -- a &rp *rp &rp-1)
    DUP RP!      \ set rstack ptr to next
    !            \ store cur return addr in       (a &rp *rp &rp-1 -- a &rp)
                 \ next position (so it's on top)
    !            \ store `a` in *rsp              (a &rp -- )
;

\ Push to data stack from return stack
: R> ( R:a -- a )
    RP@ CELL+ @  \ get data before current return addr   ( -- a )
    RP@ @        \ get current return addr               ( a -- a *rp )
    RP@ CELL+    \ get addr below top of stack           ( a *rp -- a *rp &rp-1 )
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
    CELLS SP@ CELL+ + @
;
: 3DUP  ( a b c -- a b c a b c )
    2 PICK 2 PICK 2 PICK ;
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
: DEPTH     SP0 SP@ - CELL- CELL / ;
: RDEPTH    RP0 RP@ - CELL / ;

\ Align input to cell size (e.g. 3 -> 8, 15 -> 16)
: ALIGNED ( a -- a )
    CELL 1- +
    CELL 1- INVERT
    AND
;

: ?ALIGNED ( a -- a )
    CELL 1-
    AND
    0=
;

\ Update HERE to be cell aligned
: ALIGN  ( -- )
    HERE @ ALIGNED
    HERE !
;

: ?interpreting ( -- a )
    STATE @ 0= ;
: ?compiling ( -- a )
    STATE @ ;

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
        CELL-          \ placeholder one cell before start of string
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

\ val >= lo && val < hi
: WITHIN ( val lo hi -- a )
    2 PICK >       \ ( val lo hi val --> val lo <hi )
    -ROT >=        \ ( <hi val lo    --> <hi >=lo)
    AND
;

\ Compile a reference to the word currently being defined.
: RECURSE IMMEDIATE ( -- )
    LATEST @ >CFA ,
;

\ CASE c1 OF v1 ENDOF c2 OF v2 ENDOF ... default ENDCASE
\
\ This word pushes a marker word to the stack to be consumed by the child
\ branches.
\
\ Compiles into an IF/ELSE/THEN chain.
: CASE IMMEDIATE ( -- 0 )
    0
;

\ ... cond OF value ENDOF ...
: OF IMMEDIATE ( 0 ... -- 0 ... if-marker )
    ' OVER ,
    ' = ,
    [COMPILE] IF
    ' DROP ,
;

\ ... cond OF value ENDOF ...
: ENDOF IMMEDIATE ( 0 ... if-marker -- 0 ... else-marker )
    [COMPILE] ELSE
;

\ Pops the if/else jump positions until we reach the 0 placed by CASE.
: ENDCASE IMMEDIATE ( 0 ... marker -- )
    ' DROP ,
    BEGIN
        ?DUP
    WHILE
        [COMPILE] THEN
    REPEAT
;

: >BODY ( xt -- a-addr )
    5 CELLS +
;

\ Copy u bytes from c-from to c-to.
\ The memory regions must not be overlapped.
: memcpy ( c-from c-to u -- )
    BEGIN
        DUP 0>
    WHILE
        1- >R   \ decrement u, save
        OVER C@
        OVER C! \ copy character
        1+ >R   \ increment c-to, save
        1+      \ increment c-from
        R> R>
    REPEAT
    3DROP
;

: memcpy, ( addr-from size -- )
    TUCK           \ (.. -- size a-from size)
    HERE @ SWAP    \ (.. -- size a-from a-here size)
    memcpy         \ Copy `size` bytes from `a-from` to `HERE`
    HERE +!        \ Increment *here
;

\ Print name of the word
: ID. ( nt -- )
    CELL+ 1+
    DUP C@
    BEGIN
        DUP 0>
    WHILE
        SWAP
        1+ DUP C@ EMIT
        SWAP 1-
    REPEAT
    2DROP
;

: immediate-bit 1 ;
: hidden-bit    2 ;

\ Print all known (and not hidden) words
: WORDS
    LATEST @
    BEGIN
        ?DUP
    WHILE
        DUP
        CELL+ C@ hidden-bit AND UNLESS
            DUP ID. SPACE
        THEN
        @
    REPEAT
    CR
;

\ Create a new named word which pushes its own address to the stack when called.
\
\ This can be used as a primitive to build variables, constants, etc.
: CREATE ( -- )
    ALIGN
    LATEST @ ,               \ prev word link
    HERE @ CELL- LATEST !    \ update latest
    0 C,                     \ flags byte
    BL WORD
    DUP C,                   \ length byte
    memcpy,                  \ copy name
    ALIGN                    \ add padding
    DOCOL ,                  \ compile docol
    ' LIT ,
    HERE @ 3 CELLS + ,       \ compile the address after the EXIT
    ' NOP ,                  \ DOES>, can modify this later
    ' EXIT ,                 \ compile exit
;

\ :NONAME body-part ; EXECUTE
\
\ Anonymous (unnamed) word. Leaves an "execution token" on the stack which can
\ be EXECUTE'd or compiled to run the body of the word.
: :NONAME ( -- xt )
    ALIGN
    LATEST @ ,             \ back-link
    HERE @ CELL- LATEST !  \ update word
    0 C,                   \ flags byte
    0 C,                   \ length byte
    ( notice: no name here )
    ALIGN                  \ padding
    HERE @                 \ the execution token (= DOCOL entry)
    [ DOCOL ] LITERAL ,    \ compile DOCOL
    ]                      \ start compiling body
;

\ Runtime helper for DOES>
\
\ Swaps out the NOP of a CREATE'd word with xt from the stack
: (does>) ( xt -- )
    LATEST @ >CFA  \ DOCOL addr of last defined word
    3 CELLS +      \ Point to addr of NOP
    !              \ Swap it out with the xt
;

\ Magic trick which allows the call behavior of a word to be changed at runtime
\
\   CREATE a 0 , DOES> @ 1+ ;
\   a @    --> 0
\   a      --> 1
\   1 a !  --> 1
\   a      --> 2
: DOES> IMMEDIATE
    ALIGN
    0                  \ Placeholder, will store xt from :NONAME call
    [COMPILE] LITERAL  \ (run) push value in prev cell
    HERE @ CELL-       \ Push pointer to the placeholder cell

    ' (does>) ,        \ (run) rewrite NOP addr with :NONAME xt
    [COMPILE] ;        \ (run) finish compilation of DOES>

    :NONAME            \ (comp) write body of DOES> into anon word
    SWAP !             \ (comp) (xt &placholder)
;

\ val CONSTANT name
\
\ Define a word (immediate value) which pushes a constant value to the stack
: CONSTANT ( val -- )
    CREATE , DOES> @
;

\ "allocate memory" by incrementing the data pointer by n bytes
: ALLOT ( n -- ) HERE +! ;

\ Allocate a cell of memory as a named variable
\
\ Define: VARIABLE foo
\ Read  : foo @
\ Write : 1 foo !
: VARIABLE CREATE 0 , ;

VARIABLE exc-handler

\ TODO: document me
\
\ CATCH and THROW are from the implementation given in
\ https://forth-standard.org/standard/exception/THROW
: CATCH ( xt -- exception# | 0 )
   SP@ >R                ( xt )       \ save data stack pointer
   exc-handler @ >R      ( xt )       \ and previous handler
   RP@ exc-handler !     ( xt )       \ set current handler
   EXECUTE               ( )          \ execute returns if no THROW
   R> exc-handler !      ( )          \ restore previous handler
   R> DROP               ( )          \ discard saved stack ptr
   0                     ( 0 )        \ normal completion
;

: THROW ( ??? exception# -- ??? exception# )
    ?DUP IF                 ( exc# )     \ 0 THROW is no-op
      exc-handler @ RP!     ( exc# )     \ restore prev return stack
      R> exc-handler !      ( exc# )     \ restore prev handler
      R> SWAP >R            ( saved-sp ) \ exc# on return stack
      SP! DROP R>           ( exc# )     \ restore stack

      \ Return to the caller of CATCH because return
      \ stack is restored to the state that existed
      \ when CATCH began execution
    THEN
;

: RDROP R> RP@ ! ;
: RPICK CELLS RP@ + CELL + @ ;

: I 2 RPICK ;

1 CONSTANT DO-MARK
2 CONSTANT LEAVE-MARK

CREATE DO-STACK 16 CELLS ALLOT
VARIABLE DO-IDX
    DO-STACK 16 CELLS + DO-IDX !

\ push `a` onto the `do-stack`
: PUSH-DO ( a -- do: a )
    CELL DO-IDX -!
    DO-IDX @ !
;

\ pop `a` from the `do-stack`
: POP-DO ( do: a -- a )
    DO-IDX @ @
    CELL DO-IDX +!
;

\ peek at top of `do-stack`
: PEEK-DO
    DO-IDX @ @
;

\ limit initial DO loop-part LOOP
\ limit initial DO loop-part increment +LOOP
: DO IMMEDIATE ( C: -- do-sys ) ( limit init -- )
    ' >R , ' >R ,   \ save init and limit
    HERE @ PUSH-DO
    DO-MARK PUSH-DO
;

: LOOP IMMEDIATE ( C: do-sys -- ) ( -- ) ( R: loop-sys -- )
    ' R> ,   ' R> , ' 1+ ,  \ restore limit and index, increment index
    ' 2DUP , ' >R , ' >R ,  \ copy and save
    ' = , ' 0BRANCH ,       \ if we haven't reached the end
    POP-DO DROP             \   ignore the marker
    POP-DO HERE @ - ,       \   compile distance from matching `DO`
    ' RDROP ,               \ drop start + limit
    ' RDROP ,
;

\ Because +LOOP has an arbitrary increment, we can't check for exact equality.
\ We don't know the starting value here, just the current index and the "limit",
\ so the logic is to tell if we've crossed over that value on this iteration
\
\ Wizardry stolen from planckforth
: ?LOOP-END
    SWAP -
    2DUP +
    OVER XOR
    >R XOR R>
    AND 0<
;

: +LOOP IMMEDIATE ( C: do-sys -- ) ( -- ) ( R: loop-sys -- )
    ' R> , ' R> , ' 3DUP ,      \ restore limit and index and copy ( incr limit index )
    ' ROT , ' + ,               \ apply the increment              ( limit index+incr )
    ' >R , ' >R ,               \ save limit and index             ( -- incr limit index )
    ' ?LOOP-END , ' 0BRANCH ,   \ if we haven't reached the end
    POP-DO DROP                 \   ignore the marker
    POP-DO HERE @ - ,           \   compile distance from matching `DO`
    ' RDROP ,                   \ drop start + limit
    ' RDROP ,
;
