VARIABLE actual-depth
CREATE actual-stack 32 CELLS ALLOT

: Term.red  27 EMIT ." [0;31m" ;
: Term.yellow 27 EMIT ." [0;33m" ;
: Term.green 27 EMIT ." [0;32m" ;
: Term.reset 27 EMIT ." [0m" ;

\ Given a base addr and count, find the first instance of T{ before addr+u
: find-start  ( addr u -- addr' u' )
    OVER +                   ( addr addr-end )
    TUCK                     ( addr-end addr-start addr-ptr )
    BEGIN
        1-                   ( addr-end addr-start addr-ptr )
        2DUP <               \ while addr-ptr >= addr-start
    WHILE
        DUP C@               \ See if we're pointing at a "T{"
            [CHAR] { =
        OVER 1- C@
            [CHAR] T =
        AND IF               \ We are! Top of stack points to "{"
            NIP              \ Get rid of base addr
            1-               \ Move back to the "T"
            SWAP OVER 1+ -   \ See how far we've moved from start of string, trim NL
            EXIT
        THEN
    REPEAT
    DROP

    0
;

\ Display error message followed by line that produced the error
: locate-error
    \ Search backward for matching T{
    SOURCE find-start
    ?DUP IF
        TELL CR
    ELSE
        ." no matching T{ found" CR
    THEN
;

VARIABLE #pass 0 #pass !
VARIABLE #fail 0 #fail !

: passed! 1 #pass +! ;
: failed!
    1 #fail +!
    CR Term.red ." fail "
    Term.yellow locate-error Term.reset
;

\ T{ setup-part -> assert part }T
: T{ ( -- ) ;

: ->
    DEPTH actual-stack SWAP
    DUP actual-depth !

    BEGIN
        ?DUP
    WHILE
        -ROT DUP CELL+ >R ! R> SWAP 1-
    REPEAT
    DROP
;

: }T
    DEPTH actual-depth @ != IF
        failed!

        ."     stack effect incorrect " CR
        ."       have depth: " actual-depth @ . CR
        ."       want depth: " DEPTH . CR
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
            failed!

            ."     value mismatch at offset " 3 PICK . CR
            ."       expected: " . CR
            ."       actual: " . CR
            R> R> SWAP
            DROP
            FALSE >R >R
        ELSE
            2DROP
        THEN
        1-             \ decrement counter
        R> CELL+
        SWAP
    REPEAT
    DROP               \ drop array pointer
    R> IF
        ." ."
        passed!
    ELSE
        SP0 SP!
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
T{ 1 2 2DUP    -> 1 2 1 2 }T
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
T{ ?interpreting ?compiling -> 1 0 }T
T{ : tt ?interpreting [ ?compiling ] ; tt -> 0 1 }T
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
T{ : [c1] [COMPILE] DUP ; IMMEDIATE -> }T
T{ 123 [c1] -> 123 123 }T
T{ : [c2] [COMPILE] [c1] ; -> }T
T{ 234 [c2] -> 234 234 }T
T{ : [cif] [COMPILE] IF ; IMMEDIATE -> }T
T{ : [c3]  [cif] 111 ELSE 222 THEN ; -> }T
T{ -1 [c3] -> 111 }T
T{  0 [c3] -> 222 }T

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
T{ :NONAME DUP IF DUP >R 1- RECURSE R> THEN ;
   3 SWAP EXECUTE -> 0 1 2 3 }T

CR ." ===[CASE]===" CR
: cs1
    CASE
        1 OF 111 ENDOF
        2 OF 222 ENDOF
        3 OF 333 ENDOF
        >R 999 R>
    ENDCASE
;

T{ 1 cs1 -> 111 }T
T{ 2 cs1 -> 222 }T
T{ 3 cs1 -> 333 }T
T{ 4 cs1 -> 999 }T

CR ." ===[:NONAME]===" CR
VARIABLE nn1
VARIABLE nn2
T{ :NONAME 123 ; EXECUTE -> 123 }T
T{ :NONAME 1234 ; nn1 ! -> }T
T{ :NONAME 9876 ; nn2 ! -> }T
T{ nn1 @ EXECUTE -> 1234 }T
T{ nn2 @ EXECUTE -> 9876 }T

CR ." ===[CREATE + DOES>]===" CR
: add-1 DOES> @ 1 + ;
: add-2 DOES> @ 2 + ;
T{ CREATE tt ->        }T
T{ tt        -> HERE @ }T
T{ 1 ,       ->        }T
T{ tt @      -> 1      }T
T{ add-1     ->        }T
T{ tt        -> 2      }T
T{ add-2     ->        }T
T{ tt        -> 3      }T

: (ctr)
    CREATE 0 ,
    DOES>
        DUP @ ( addr val )
        1 ROT ( addr val 1 -- val 1 addr )
        +! ;
T{ (ctr) x (ctr) y -> }T
T{ x x             -> 0 1 }T
T{ y               -> 0 }T

:NONAME
    #fail @ IF
        Term.red
    ELSE
        Term.green
    THEN

    CR ." Finished with "
        #fail @ . ."  failures out of "
        #pass @ . ."  tests." CR

    Term.reset
; EXECUTE
