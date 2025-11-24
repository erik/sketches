VARIABLE actual-depth

\ FIXME: jank
ALIGN
HERE @
20 CELLS HERE +!
ALIGN
CONSTANT actual-stack

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
        ."   have depth: " DEPTH . CR
        ."   want depth: " actual-depth @ . CR
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
        ." TEST PASSED" CR
    ELSE
        ." TEST FAILED" CR
    THEN
;

." ===[Test framework]===" CR
T{ -> }T
T{ 1 2 3 -> 1 2 3 }T

." ===[Binary Operations]===" CR
T{ 2 1 > -> TRUE }T
T{ 2 1 >= -> TRUE }T
T{ 2 2 >= -> TRUE }T

." ===[Stack Manipulation]===" CR
T{ 1 2 3 ROT   -> 2 3 1 }T
T{ 1 2 3 -ROT  -> 3 1 2 }T
T{ 1 2 NIP     -> 2 }T
T{ 1 2 TUCK    -> 2 1 2 }T
T{ 0 ?DUP      -> 0 }T
T{ 1 ?DUP      -> 1 1 }T
T{ 1 >R R>     -> 1 }T
T{ DEPTH DEPTH -> 0 1 }T

." ===[Miscellaneous Words]===" CR
T{ 1 0 5 WITHIN -> 1 }T
T{ 1 1 5 WITHIN -> 1 }T
T{ 1 2 5 WITHIN -> 0 }T
T{ 5 2 5 WITHIN -> 0 }T
T{ 6 2 5 WITHIN -> 0 }T
T{ ?immediate   -> 1 }T
T{ 3 ALIGNED    -> 8 }T

." All tests completed." CR
