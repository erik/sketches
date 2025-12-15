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

\ Now let's build a simple version of block comments. We'll replace this
\ definition later with one that balances parens so we can nest them.
\
\ Implementation is the same as line comments, but using 41 (i.e. ')') as the
\ delimiter.
: ( IMMEDIATE
    [ HERE @ ] KEY 41 = 0BRANCH [ HERE @ - , ]
;

( Now it'll be easier to build more of the language in an understandable way. Onward!)

\ Pop a value off the stack and compile it into `LIT [val]`
: LITERAL IMMEDIATE ( a -- )
    ' LIT , ,
;

\ We don't have a native concept of characters yet ('\n' is simply a word called
\ "'\n'" which pushes 0x0A to the stack)
: '\n' 10 ; ( -- a )
: BL   32 ; \ space

\ Later on we want to swap out the implementations of the native words KEY and
\ WORD. We can't (easily) modify the assembly implementations, so we add a
\ layer of indirection here that's easier to swap.
\
\ NOTE: this isn't a recursive call, it compiles the reference to the OLD
\ definition of the word.
: KEY  ( -- a ) KEY ;
: WORD ( -- addr len ) WORD ;

\ Return first character of word after this call.
\
\ CHAR abc => a
: CHAR ( -- a )
    BL WORD
    DROP     \ drop len (TODO: zero case?)
    C@
;

\
\ Conditional logic
\

\ cond IF true-path THEN
\
\ This is adding some structure to the 0BRANCH approach we did above to define
\ line comments. We don't want to have to count the number of words to jump by
\ hand (it could change!), so we use THEN to rewrite the code compiled by IF
\ based on how far we've moved (tracked by HERE).
: IF IMMEDIATE ( cond -- cond offset )
    ' 0BRANCH ,     \ compile 0BRANCH
    HERE @          \ push our current position to the stack
    0 ,             \ compile placeholder jump offset (overwritten later by THEN)
;

\ Finalizer for IF
: THEN IMMEDIATE ( offset_addr -- )
    DUP             \ dup previous position set by IF
    HERE @          \ push current position to the stack
    SWAP -          \ how far have we moved? (curr - prev)
    SWAP !          \ *offset_addr = (curr - prev)
;

\ cond IF true-part ELSE false-part THEN
: ELSE IMMEDIATE ( if_offset_addr -- else_offset_addr )
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
: BEGIN IMMEDIATE ( -- jmp_offset_addr )
    HERE @          \ push current position to stack
;

\ Finalizer for BEGIN.
: UNTIL IMMEDIATE ( jmp_offset_addr cond -- )
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

\ '[COMPILE] word' compiles a word that would otherwise be executed immediately
\
\ Conceptually similar to `' word ,` if `word` is immediate.
: [COMPILE] IMMEDIATE
    BL WORD
    FIND
    >CFA ,
;

\ "compile time tick". Leaves execution token (addr) of word on the stack
: ['] IMMEDIATE ( -- xt )
    ' LIT ,
;

\ Compile-time CHAR
: [CHAR] IMMEDIATE
    CHAR
    [COMPILE] LITERAL
;

\
\ Basic helper utils
\

: NOP ;
: CELL+ ( a -- a+CELL ) CELL + ;
: CELL- ( a -- a-CELL ) CELL - ;
: CELLS ( a -- a*CELL) CELL * ;
: TRUE  ( -- a )     -1 ;
: FALSE ( -- a )      0 ;
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

\ Copy head of return stack to data stack
: R@ ( R: a -- a )
    RP@ CELL+ @
;

: RDROP ( R:a -- ) R> RP@ ! ;
: RPICK ( R:xn x0 n -- xn ) CELLS RP@ + CELL + @ ;

\ Simple stack effects, best described by their signature
: NIP   ( a b -- b )          SWAP DROP ;
: TUCK  ( a b -- b a b )      SWAP OVER ;
: 2DUP  ( a b -- a b a b )    OVER OVER ;
: 2DROP ( a b -- )            DROP DROP ;
: 3DROP ( a b c -- )          2DROP DROP ;
: ROT   ( a b c -- b c a )    >R SWAP R> SWAP ;
: -ROT  ( a b c -- c a b )    SWAP >R SWAP R> ;

\ Grab the n'th value from the stack and put it on top
: PICK  ( x0 ... xn i -- x0 ... xi x0 )
    CELLS SP@ CELL+ + @ ;

: 3DUP  ( a b c -- a b c a b c )
    2 PICK 2 PICK 2 PICK ;

\ DUP if top of stack is not zero
: ?DUP  ( 0 -- 0 | x -- x x )
    DUP DUP 0 = IF DROP THEN ;

\
\ Comparisons
\

: >   ( a b -- c )  SWAP < ;
: <=  ( a b -- c )  > NOT ;
: >=  ( a b -- c )  < NOT ;
: 0=  ( a -- b )    0 = ;
: 0<  ( a -- b )    0 < ;
: 0>  ( a -- b )    0 > ;
: 0<= ( a -- b )    0 <= ;
: 0>= ( a -- b )    0 >= ;

: MAX ( a b -- c ) 2DUP > IF DROP ELSE NIP THEN ;
: MIN ( a b -- c ) 2DUP < IF DROP ELSE NIP THEN ;

\ Now that we have better control flow constructs, let's reimplement block
\ comments so they can be nested.
: ( IMMEDIATE
    1                         \ nested level
    BEGIN
        KEY
        DUP [CHAR] ( = IF
            DROP 1+           \ increase depth
        ELSE [CHAR] ) = IF
            1-                \ decrease depth
        THEN THEN

        ?DUP 0=               \ do we have matched parens?
    UNTIL
;

( now we have
    ( nested )
  comment syntax! )

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

\ Align `a` to size `b`
\ 1 4 aligned-by -- 4
\ ( a b-1 & ~(b-1) )
: aligned-by ( a b -- a' )
    1- DUP
    INVERT
    -ROT +
    AND
;

\ Align HERE by `a`
: align-by ( a -- )
    HERE @ SWAP aligned-by
    HERE !
;

\ Align input to cell size (e.g. 3 -> 8, 15 -> 16)
: ALIGNED ( a -- a )
    CELL aligned-by
;

\ Update HERE to be cell aligned
: ALIGN  ( -- )
    CELL align-by
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
            DUP [CHAR] " !=
        WHILE
            C,
        REPEAT
        0 C,           \ add a null byte for good measure
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
            DUP [CHAR] " !=
        WHILE
            OVER C!
            1+
        REPEAT
        DROP           \ ignore final quote
        0 OVER 1+ C!   \ add null byte for good measure
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
            DUP [CHAR] " !=
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

\ ... lo hi RANGEOF value ENDOF ...
: RANGEOF IMMEDIATE ( 0 ... -- 0 ... if-marker )
    ' LIT , 2 ,
    ' PICK ,
    ' -ROT ,
    ' WITHIN ,
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
        1- >R     \ decrement u, save
        OVER C@
        OVER C!   \ copy character
        1+ >R     \ increment c-to, save
        1+        \ increment c-from
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

: ON  ( addr -- ) TRUE SWAP ! ;
: OFF ( addr -- ) FALSE SWAP ! ;

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

: ABORT -1 THROW ;

: I 2 RPICK ; \ iteration of inner-most loop param
: J 4 RPICK ; \ ... 2nd loop
: K 4 RPICK ; \ ... 3rd loop

1 CONSTANT DO-MARK
\ TODO: implement leave
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

\ DO will always loop at least once, but ?DO will skip if there's nothing to
\ do.
: ?DO IMMEDIATE ( C: -- do-sys ) ( limit init -- )
    ' 2DUP , ' = , [COMPILE] IF
        ' 2DROP ,
        ' EXIT ,
    [COMPILE] THEN

    ' >R , ' >R ,   \ save init and limit
    HERE @ PUSH-DO
    DO-MARK PUSH-DO
;

\ Check loop bounds and jump back to the top
: LOOP IMMEDIATE ( C: do-sys -- ) ( -- ) ( R: loop-sys -- )
    ' R> ,   ' R> , ' 1+ ,  \ restore limit and index, increment index
    ' 2DUP , ' >R , ' >R ,  \ copy and save
    ' = , ' 0BRANCH ,       \ if we haven't reached the end
    POP-DO DROP             \   ignore the marker
    POP-DO HERE @ - ,       \   compile distance from matching `DO`
    ' RDROP ,               \ drop start + limit
    ' RDROP ,
;

\ Pop the loop control parameters off the return stack so the loop can be
\ cleanly exited early.
: UNLOOP IMMEDIATE ( -- ) ( R: loop-sys -- )
    ' RDROP ,
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

: DECIMAL ( -- ) 10 BASE ! ;
: HEX ( -- )     16 BASE ! ;

: ?digit     ( ch -- bool ) [CHAR] 0 [CHAR] 9 1+ WITHIN ;
: ?lowercase ( ch -- bool ) 97 122 1+ WITHIN ;
: ?uppercase ( ch -- bool ) 65 90 1+ WITHIN ;

: parse-uint ( addr n base -- val ok? )
    ROT >R >R \ save base + addr ( R: addr base )
    0         \ accumulator
    SWAP
    BEGIN
        ?DUP
    WHILE    ( acc n )
        SWAP          ( n acc )
        R>            \ restore base
        DUP ROT       ( base acc base )
        * SWAP        \ acc *= base           ( acc' base )
        R>            \ restore addr
        DUP C@        \ get next char         ( acc n base addr ch )
        -ROT 1+       \ increment addr
        >R >R         \ save base, addr       ( acc n ch )

        \ Get decimal value of character
        DUP ?digit     IF [CHAR] 0 - ELSE
        DUP ?lowercase IF [CHAR] a - 10 + ELSE
        DUP ?uppercase IF [CHAR] A - 10 + THEN THEN THEN

        DUP R@           \ restore base
        > IF             \ check if the character is invalid in the base
            DROP 2DROP   \ clean up and exit with 0
            RDROP RDROP
            0 FALSE
            EXiT
        THEN

        +             \ acc += val ( n acc )
        SWAP 1-       \ decr string length
    REPEAT
    RDROP RDROP

    TRUE              \ success
;

: escaped-char ( ch -- val )
    CASE
        [CHAR] a OF  7 ENDOF  \ alert
        [CHAR] b OF  8 ENDOF  \ backspace
        [CHAR] t OF  9 ENDOF  \ tab
        [CHAR] n OF 10 ENDOF  \ newline
        [CHAR] v OF 11 ENDOF  \ vertical tab
        [CHAR] f OF 12 ENDOF  \ formfeed
        [CHAR] r OF 13 ENDOF  \ carriage return
        [CHAR] e OF 27 ENDOF  \ escape
        [CHAR] ' OF 39 ENDOF  \ single quote
        [CHAR] \ OF 92 ENDOF  \ backslash
        ( else )
            0 SWAP
    ENDCASE
;

\ Attempt to parse a number from string at `addr`.
\
\ NOTE: Doesn't follow Forth standard signature (because it feels too cumbersome to
\ me?)
: >NUMBER ( addr n -- val ok? )
    ?DUP UNLESS          \ handle empty string
        DROP
        0 FALSE
        EXIT
    THEN

    SWAP DUP C@     \ get first char of str

    CASE
        [CHAR] - OF       \ negative
            1+ SWAP 1-
            RECURSE IF
                INVERT 1+ TRUE
            ELSE
                FALSE
            THEN
        ENDOF

        [CHAR] $ OF        \ hex
            1+ SWAP 1-
            16 parse-uint
        ENDOF

        [CHAR] # OF        \ decimal
            1+ SWAP 1-
            10 parse-uint
        ENDOF

        [CHAR] ' OF        \ character code
            1+ SWAP 1-     ( addr size )

            \ Check we have at least 2 more char (the actual char plus closing
            \ quote, or \escaped-char plus closing quote)
            DUP 2 4 WITHIN UNLESS
                2DROP      \ drop addr + size
                0 FALSE    \ return value
                EXIT
            THEN

            1-            \ decr size for next char
            SWAP DUP C@   ( size addr ch )
            DUP [CHAR] \ = IF
                DROP
                SWAP 1- SWAP
                1+ DUP C@    \ get escaped char
                escaped-char ?DUP UNLESS
                    2DROP
                    0 FALSE
                    EXIT
                THEN
            THEN

            -ROT               ( ch size addr )
            1+ C@ [CHAR] ' =   \ next char is a quote
            SWAP 1- 0=         \ and it's the last one
            AND
        ENDOF

        DROP SWAP
        BASE @      \ fall back to current base
        parse-uint

        0           \ ENDCASE drops top of stack
    ENDCASE
;

\
\ Stage 2 interpreter
\

CREATE word-buffer 64 CELLS ALLOT
VARIABLE word-len

\ Forth REPL, this time hosted in Forth
: INTERPRET ( -- )
    BL WORD ?DUP UNLESS
        ." TODO: zero len word" CR
        ABORT
    THEN

    DUP word-len !             \ Update len
    2DUP word-buffer TUCK DROP ( addr len addr buf len )
    memcpy                     \ copy word into word buffer

    FIND ?DUP IF                                  \ Found the word
        ?compiling IF                             \ compile mode
            DUP CELL+ C@ immediate-bit AND IF     \ execute immediate word
                >CFA EXECUTE
            ELSE                                  \ compile the word
                >CFA ,
            THEN
        ELSE                                      \ immediate mode
            >CFA EXECUTE
        THEN
    ELSE
        word-buffer word-len @ >NUMBER UNLESS
            CR ." undefined word: '"
            word-buffer word-len @ TELL ." '" CR

            ABORT
        THEN

        \ If we're interpreting we leave the number on the stack
        ?compiling IF
            [COMPILE] LITERAL
        THEN
    THEN
;

: QUIT
    RP0 RP!
    BEGIN
        \ TODO: ?interpreting IF ." > " THEN

        ['] INTERPRET CATCH
        CASE
            0 OF
                \ TODO: ?interpreting IF ." ok." CR THEN
            ENDOF

            -1 OF
                ." [ aborted ]" CR
                RECURSE
            ENDOF

            -2 OF
                ." [ EOF ]" CR
                EXIT
            ENDOF

            ( else )
            DUP ." Exception # " .
        ENDCASE
    AGAIN
;

\ Enter second stage interpreter - we're self-hosted baby
QUIT

\ Counted string to null-terminated string.
: >c-str ( addr u -- c-str ) DROP ;

\ Push length of null terminated string to stack
: c-str> ( addr -- addr len )
    0
    BEGIN
        2DUP + C@
    WHILE
        1+
    REPEAT
;

0 CONSTANT R/O
1 CONSTANT W/O
2 CONSTANT R/W

1 CONSTANT sys-exit  \ void exit(int rval)
2 CONSTANT sys-fork  \ int fork(void)
3 CONSTANT sys-read  \ ssize_t read(int fd, void *buf, size_t count)
4 CONSTANT sys-write \ ssize_t write(int fd, const void *buf, size_t count)
5 CONSTANT sys-open  \ int open(const char *path, int flags, mode_t mode)
6 CONSTANT sys-close \ int close(int fd)

0 CONSTANT success

: die ( val -- ) sys-exit SYSCALL1 ;
: BYE ( -- ) success die ;

\ TODO: this doesn't handle non-existent files correctly
: OPEN-FILE ( addr u mode -- fd ok )
    >R >c-str R> SWAP
    sys-open SYSCALL2
    DUP 0< IF DUP ELSE success THEN
;

: CLOSE-FILE ( fd -- ok )
    sys-close SYSCALL1 0< IF DUP ELSE success THEN
;

: READ-FILE ( addr len fd -- len ok )
    >R SWAP R>          \ read(fd, &addr, len)
    sys-read SYSCALL3
    DUP 0< IF DUP ELSE success THEN
;

: STRUCT{ 0 ;
: CELL% ( -- align size ) CELL CELL ;
: CHAR% ( -- align size ) 1 1 ;
: BYTE% 1 1 ;
: PTR% CELL% ;
: INT% CELL% ;
: I32% 4 4 ;
: U32% 4 4 ;
: I16% 2 2 ;
: U16% 2 2 ;

: FIELD ( align size -- addr )
    -ROT aligned-by     \ ( size offset )
    CREATE
        DUP ,           \ fill offset
        +               \ new offset
    DOES> @ +
;

: }STRUCT
    CREATE
        ,
    DOES>
        @
        CELL SWAP
;

: %ALLOT ( align size -- addr )
    HERE @
    -ROT SWAP
    align-by ALLOT
;

1024 CONSTANT BUFSIZE
0    CONSTANT stdin
0    CONSTANT EOF

STRUCT{
    cell%           FIELD magic
    cell%           FIELD stream>fd
    char% 128 *     FIELD stream>name
    char% BUFSIZE * FIELD stream>buf
    cell%           FIELD stream>buf-pos
    cell%           FIELD stream>buf-end
    cell%           FIELD stream>lineno
    ptr%            FIELD stream>next
}STRUCT stream%

VARIABLE streams
    0 streams !

: push-stream ( fd -- )
    stream% %allot            \ ( fd addr )
    $CAFEBABE OVER magic !
    TUCK stream>fd !          \ ( addr )
    0 OVER stream>buf-pos !
    0 OVER stream>buf-end !
    0 OVER stream>lineno !
    streams @ OVER stream>next !
    streams !
;

: pop-stream ( -- fd )
    streams @ DUP
    stream>next @ streams !
    stream>fd
;

\ Get the length in bytes of the data currently in this stream's
\ buffer.
: stream-buffer-count ( s -- len )
    DUP stream>buf-end @ SWAP stream>buf-pos @ -
;

: stream-inspect ( s -- s )
    ." stream { " cr
    ."    base: " DUP . cr
    ."    magic " DUP @ . cr
    ."    fd:   " DUP stream>fd DUP . @ .  cr
    ."    name: " DUP stream>name c-str> TELL cr
    ."    buf:  " DUP stream>buf DUP . @ . cr
    ."    bpos  " DUP stream>buf-pos DUP . @ . cr
    ."    bend  " DUP stream>buf-end DUP . @ . cr
    ."    line  " DUP stream>lineno DUP . @ . cr
    ."    next  " DUP stream>next DUP . @ . cr
    ." }" cr
;

: stream-buffer-clear ( s -- )
    0 OVER stream>buf-end !
    0 SWAP stream>buf-pos !
;

: stream-buffer-refill ( s -- len e )
    DUP >R

    DUP stream-buffer-clear

    stream>buf
        OVER stream>fd @
        BUFSIZE
        SWAP
        READ-FILE

    OVER R> stream>buf-end !
;

: stream-buffer-read ( to-addr len s -- len )
    DUP stream>buf-pos @ OVER stream>buf +
        3 PICK             \ addr
        3 PICK             \ len
        memcpy

    stream>buf-pos OVER SWAP +!
    NIP
;


\ Read from already open file descriptor
: INCLUDE-FILE ( fd -- )
    push-stream

    \ We can't know the name from the file descriptor, caller can overwrite
    \ later
    s" (INCLUDE-FILE fd)" streams @ stream>name SWAP memcpy
;


\ Read from a named file
: INCLUDED ( addr u -- )
    2DUP R/O OPEN-FILE THROW
        push-stream

    streams @ stream>name SWAP memcpy
;

\ Convenience wrapper for INCLUDED, read from a named word
: INCLUDE ( -- ) BL WORD INCLUDED ;

STRUCT{
    ptr%        FIELD required>next
    char% 128 * FIELD required>name
}STRUCT required%

VARIABLE required-files
    0 required-files !

: streq? ( addr1 u addr2 u -- b )
    \ Check string len first
    ROT ( a1 a2 u u )
    DUP = UNLESS
        3DROP
        FALSE EXIT
    THEN

    0 DO
        ( a1 a2 )
        DUP C@        ( a1 a2 c2 )
        ROT DUP C@   ( a2 c2 a1 c1 )
        ROT = UNLESS
            2DROP
            UNLOOP
            FALSE EXIT
        THEN
        1+ SWAP 1+
    LOOP

    2DROP
    TRUE
;

: required? ( addr u -- b )
    required-files @
    BEGIN
        ?DUP UNLESS
            2DROP
            FALSE EXIT
        THEN

        ( addr u ptr )
        DUP required>name c-str> ( addr u ptr name len )
        4 PICK 4 PICK            ( addr u ptr name len addr u )
        streq? IF
            DROP 2DROP
            TRUE EXIT
        THEN

        required>next @
    AGAIN
;

\ Read from a named file, but dedupe so it's only done once.
: REQUIRED ( addr u -- )
    2DUP required? IF
        2DROP
    ELSE
        2DUP

        required% %allot

        DUP required-files @ SWAP required>next !
        DUP required-files !

        ( addr len ptr )
        required>name SWAP memcpy

        INCLUDED
    THEN
;

\ Just like `INCLUDE`, but checking if the import has already been done.
: REQUIRE ( -- ) BL WORD REQUIRED ;


VARIABLE source-buffer
    BUFSIZE ALLOT

VARIABLE source-buffer-pos
    0 source-buffer-pos !

VARIABLE source-buffer-end
    0 source-buffer-end !

\ TODO: clean up native definition.
: (source) ( -- addr len ) SOURCE ;

VARIABLE SOURCE-ID
    -1 SOURCE-ID !

: SOURCE ( -- addr u ) source-buffer source-buffer-end @ ;
: >IN    ( -- addr )   source-buffer-pos ;

: stream-read ( addr len s -- len err )
    \ check if we have enough buffered data to avoid calling read()
    DUP stream-buffer-count ( addr len s cnt )
    2 PICK >= IF            ( addr len s )
        stream-buffer-read
        success EXIT
    THEN

    DUP stream-buffer-count 0> IF
        \ we need to read more data, first copy whatever was in the
        \ buffer
        DUP stream>buf-pos @
        OVER stream-buffer-count DUP >R
        4 PICK SWAP memcpy             ( addr len s )
        ROT R@ + -ROT                  ( addr' len s )
        SWAP R@ - SWAP                 ( addr rem' s )
    ELSE
        0 >R
    THEN

    \ Refill the buffer, read next `BUFSIZE` bytes from stream
    DUP stream-buffer-refill

    ?DUP IF
        ." err read:" DUP . CR
        >R 2DROP
        0 R>
        EXIT
    THEN

    ?DUP UNLESS             \ check that we've read something
        3DROP
        R> SUCCESS
        EXIT
    THEN

    SWAP >R MIN R>          \ avoid reading past end of buffer
    stream-buffer-read      ( addr rem s -- len )
    R> +                    \ add whatever we transfered from the old buffer
    SUCCESS
;

VARIABLE key-buf

\ read single character from stream, return EOF on end
: stream-key ( s -- ch e )
    \ push pointer to own address to stack, we'll write our character here.
    key-buf
    1
    ROT
    stream-read THROW

    0= IF
        EOF success
        EXIT
    THEN

    key-buf C@
    success
;

: eol? ( ch -- bool )
    CASE
        EOF  OF TRUE ENDOF
        0    OF TRUE ENDOF
        '\n' OF TRUE ENDOF
        ( else)
            FALSE SWAP
    ENDCASE
;

\ Read next line of input from current file
: REFILL ( -- ok )
    streams @ 0= IF
        \ TODO: this should return up to the INTERPRET loop cleanly
        ." [EOF] exhausted all inputs" cr
        -1 die
        FALSE EXIT
    THEN

    0 source-buffer-pos !
    0 source-buffer-end !

    \ TODO: this can overflow the buffer
    BEGIN
        streams @ stream-key THROW

        DUP EOF = IF
            DROP
            pop-stream CLOSE-FILE THROW
            RECURSE EXIT
        THEN

        DUP
        source-buffer source-buffer-end @ + c!
        1 source-buffer-end +!
    EOL? UNTIL

    TRUE
;

\ Reimplement KEY to pull from current buffer
: new-key ( -- a )
    source-buffer-pos @ source-buffer-end @ >= IF
        REFILL UNLESS
            EOF EXIT
        THEN
    THEN

    source-buffer
        source-buffer-pos @ +

    1 source-buffer-pos +!
    C@
;

: whitespace? ( ch -- bool )
    CASE
        '\n' OF TRUE ENDOF
        BL   OF TRUE ENDOF
        '\t' OF TRUE ENDOF
        '\r' OF TRUE ENDOF
        ( else )
            FALSE SWAP
    ENDCASE
;

: skip-whitespace ( d -- k )
    BEGIN
        KEY                ( delim key )
        DUP whitespace?    ( delim key key -- delim key is-sp )
    WHILE
        DROP               ( delim )
    REPEAT
;

\ Advance past the delimiter given. If delimiter is BL, will skip ALL
\ whitespace chars.
: skip-delim ( d -- k )
    DUP BL = IF
        DROP
        skip-whitespace
    ELSE
        BEGIN
            KEY  ( delim k )
            OVER ( delim k delim )
            OVER ( delim k delim k )
        = WHILE
            DROP ( delim )
        REPEAT
        NIP      ( k )
    THEN
;

VARIABLE word-buffer
    64 ALLOT

: new-word ( delim -- c-addr u )
    DUP skip-delim
    DUP EOF = IF
        DROP word-buffer 0
        EXIT
    THEN

    word-buffer TUCK C!
    1+                     ( delim buf )
    BEGIN
        SWAP KEY           ( buf delim k )
        OVER BL = IF
            DUP whitespace?
        ELSE
            2DUP =
        THEN               ( buf delim k is-delim? )

        IF                 \ We've found the delimiter, end the word
            2DROP
            0 SWAP C!
            word-buffer c-str>
            EXIT
        THEN
                          \ Haven't hit the delim yet, write the char and
                          \ continue
        ROT               ( delim k buf )
        2DUP c!           ( delim k buf k buf -- delim k buf )
        1+
        NIP
    AGAIN
;

\ Re-define line comments to use the updated key implementation
: \ IMMEDIATE BEGIN KEY EOL? UNTIL ;

\ Similarly, redefine creation word `:` in Forth so we don't reference the
\ native `WORD` definition.
: :
    ALIGN
    LATEST @ ,
    HERE @ CELL- LATEST !
    hidden-bit C,
    BL WORD
    DUP C,
    memcpy,
    ALIGN
    DOCOL ,
    ]
;

:NONAME
    \ Transfer remaining content of boot.f into Forth-hosted buffer.
    \
    \ This will contain everything AFTER the :NONAME block.
    (source) DUP BUFSIZE > IF
        ." [BUG] too many characters remain in boot.f: " . CR
        ABORT
    THEN

    stdin INCLUDE-FILE
    streams @
        2DUP stream>buf-end !          \ Update buffer length of Forth impl
        stream>buf SWAP memcpy         \ Copy bytes from native buffer

    s" (stdin)"
        streams @ stream>name SWAP
        memcpy

    \ Transfer control of reading to our self-hosted implementations
    ' new-key
        ' KEY CELL+ !

    ' new-word
        ' WORD CELL+ !

    \ From this point on the interpreter is fully self-hosted. The assembly
    \ implementations are no longer running
; EXECUTE

VARIABLE argc
    (argc) @ argc !

VARIABLE argv
    (argv) @ argv !

: SHIFT-ARGS ( -- )
    argc @ ?DUP UNLESS EXIT THEN
    1- argc !
    argv @ CELL+ argv !
;

: NEXT-ARG ( -- addr u )
    argc @ UNLESS 0 0 EXIT THEN
    argv @ @ c-str>
    SHIFT-ARGS
;

: ARG ( i -- addr u )
    DUP argc @ < UNLESS DROP 0 0 EXIT THEN
    CELLS argv @ + @ c-str>
;

:NONAME
    SHIFT-ARGS        \ skip past executable name

    BEGIN
        NEXT-ARG
    DUP WHILE
        2DUP ." importing: " tell cr
        INCLUDED
    REPEAT
    2DROP
; EXECUTE
