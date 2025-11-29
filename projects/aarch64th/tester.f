: NOP ;

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

\ TODO: make this the official impl
: create!
    ALIGN
    LATEST @ ,                  \ prev word link
    HERE @ cell- LATEST !       \ update latest
    0 C,                        \ flags byte
    WORD
    DUP C,                      \ length byte
    memcpy,                     \ copy name
    ALIGN                       \ add padding
    DOCOL ,                     \ compile docol
    ['] LIT ,
    HERE @ 3 CELLS + ,          \ compile the address
    ['] NOP ,                   \ DOES>, if any, will fill this cell
    ['] EXIT ,                  \ compile exit
;

VARIABLE actual-depth
create! actual-stack 20 CELLS ALLOT

\ Define a unit test syntax
\
\ T{ setup-part -> assert part }T
: T{ ( -- ) ;

: ->
    DEPTH actual-stack SWAP
    DUP actual-depth !

    BEGIN
        ?DUP
    WHILE
        -ROT DUP cell+ >R ! R> SWAP 1-
    REPEAT
    DROP
;


: }T
    DEPTH actual-depth @ != IF
        ." stack effect incorrect " CR
        ."   have depth: " actual-depth @ . CR
        ."   want depth: " DEPTH . CR
        EXIT
    THEN
    \ Keep track of success state (so we can clear the stack)

    TRUE >R
    actual-stack        \ get base of actual stack array
    actual-depth @
    BEGIN
        ?DUP
    WHILE
        SWAP
        DUP >R
        @
        ROT
        2DUP != IF
            ." value mismatch at offset " 3 PICK . CR
            ."   expected: " . CR
            ."   actual: " . CR
            R> R> SWAP
            DROP
            FALSE >R
            >R
        ELSE
            2DROP
        THEN
        1-             \ decrement counter
        R> cell+
        SWAP
    REPEAT
    DROP                \ drop array pointer
    R> IF
        ." ."
    ELSE
        CR ." TEST FAILED" CR
    THEN
;

0  CONSTANT 0S
-1 CONSTANT 1S

CR ." ===[Test framework]===" CR
T{ -> }T
T{ 1 2 3 -> 1 2 3 }T

CR ." ===[Binary Operations]===" CR
T{ 2 1 > -> TRUE }T
T{ 2 1 >= -> TRUE }T
T{ 2 2 >= -> TRUE }T
T{ 0 0 AND -> 0 }T
T{ 0 1 AND -> 0 }T
T{ 1 0 AND -> 0 }T
T{ 1 1 AND -> 1 }T

T{ 0 INVERT 1 AND -> 1 }T
T{ 1 INVERT 1 AND -> 0 }T
T{ 0 INVERT -> -1 }T
T{ -1 INVERT -> 0 }T

T{ 0S 0S XOR -> 0S }T
T{ 0S 1S XOR -> 1S }T
T{ 1S 0S XOR -> 1S }T
T{ 1S 1S XOR -> 0S }T

CR ." ===[Stack Manipulation]===" CR
T{ 1 2 OVER    -> 1 2 1 }T
T{ 5 4 3 2 1
   4 PICK      -> 5 4 3 2 1 4 }T
T{ 1 2 3 ROT   -> 2 3 1 }T
T{ 1 2 3 -ROT  -> 3 1 2 }T
T{ 1 2 NIP     -> 2 }T
T{ 1 2 TUCK    -> 2 1 2 }T
T{ 0 ?DUP      -> 0 }T
T{ 1 ?DUP      -> 1 1 }T
T{ 1 >R R>     -> 1 }T
T{ DEPTH DEPTH -> 0 1 }T

CR ." ===[Miscellaneous Words]===" CR
T{ 1 0 5 WITHIN -> 1 }T
T{ 1 1 5 WITHIN -> 1 }T
T{ 1 2 5 WITHIN -> 0 }T
T{ 5 2 5 WITHIN -> 0 }T
T{ 6 2 5 WITHIN -> 0 }T
T{ ?immediate ?compiling -> 1 0 }T
T{ : tt ?immediate [ ?compiling ] ; tt -> 0 1 }T
T{ 0 ALIGNED 7 ALIGNED 8 ALIGNED 15 ALIGNED -> 0 8 8 16 }T
T{ 1 ?ALIGNED -> FALSE }T
T{ 0 ?ALIGNED -> TRUE }T
T{ 8 ?ALIGNED -> TRUE }T

CR ." ===[Ticks and tacks]===" CR
T{ : t1 123 ;
   : t2 IMMEDIATE ['] t1 ;
   t2 EXECUTE -> 123 }T

CR ." ===[Characters]===" CR
: t1 [CHAR] XYZ ;
: t2 [CHAR] A ;
: t3 [ t2 ] LITERAL ;

T{ CHAR A -> 65 }T
T{ CHAR a -> 97 }T
T{ t1 t2 -> CHAR X CHAR A }T
T{ t3 -> CHAR A }T

CR ." ===[Interpretation / compilation semantics]===" CR
T{ : t1 123 ;           -> }T
T{ : t2 IMMEDIATE 456 ; -> }T
T{ : t3 t1 t2 ;         -> 456 }T
T{ t3                   -> 123 }T

\ TODO: implement POSTPONE
\ T{ : t4 IMMEDIATE POSTPONE t1 ;       -> }T
\ T{ : t5 t4 ;                          -> }T
\ T{ t5                                 -> 123 }T
\ T{ : t6 IMMEDIATE POSTPONE t2 ;       -> }T
\ T{ t6                                 -> 456 }T

CR ." ===[CATCH/THROW]===" CR
: t1 9 ;
: c1 1 2 3 ['] t1 CATCH ;
T{ c1 -> 1 2 3 9 0 }T    \ No THROW executed

: t2 8 0 THROW ;
: c2 1 2 ['] t2 CATCH ;
T{ c2 -> 1 2 8 0 }T      \ 0 THROW does nothing

: t3 7 8 9 99 THROW ;
: c3 1 2 ['] t3 CATCH ;
T{ c3 -> 1 2 99 }T       \ Restores stack to CATCH depth

: t4 1- DUP 0> IF RECURSE ELSE 999 THROW -222 THEN ;
: c4 3 4 5 10 ['] t4 CATCH -111 ;
T{ c4 -> 3 4 5 0 999 -111 }T        \ Test return stack unwinding

: t5 2DROP 2DROP 9999 THROW ;
: c5 1 2 3 4 ['] t5 CATCH            \ Test depth restored correctly
   DEPTH >R DROP 2DROP 2DROP R> ;    \ after stack has been emptied
T{ c5 -> 5 }T

CR ." ===[CREATE]===" CR
T{ create! CR1 -> }T
T{ CR1   -> HERE @ }T

CR ." ===[RECURSE]===" CR
: count-up
    DUP IF
        DUP >R
        1- RECURSE
        R>
    THEN
;

T{ 0 count-up -> 0 }T
T{ 4 count-up -> 0 1 2 3 4 }T

CR ." All tests completed." CR
