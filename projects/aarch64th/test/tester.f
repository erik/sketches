VARIABLE actual-depth
VARIABLE saved-depth
CREATE actual-stack 32 CELLS ALLOT
CREATE saved-stack 32 CELLS ALLOT

: Term.red    '\e' EMIT ." [0;31m" ;
: Term.yellow '\e' EMIT ." [0;33m" ;
: Term.green  '\e' EMIT ." [0;32m" ;
: Term.reset  '\e' EMIT ." [0m" ;

: restore-stack
    DEPTH IF
        ." [clearing stack]" CR
        SP0 SP!
    THEN

    saved-depth @ 0 ?DO
        saved-stack I CELLS + @
    LOOP
;

VARIABLE verbose
    verbose ON

VARIABLE #pass 0 #pass !
VARIABLE #fail 0 #fail !

: passed! 1 #pass +! ;
: failed!
    1 #fail +!
    CR Term.red ." fail "
    Term.yellow SOURCE TELL Term.reset
;

: TESTING"
    [compile] s"
    verbose @ IF
        CR
        2 SPACES TELL SPACE
    ELSE
        2DROP
    THEN
;

\ T{ setup-part -> assert part }T
: T{ ( -- )
    \ save anything currently on the stack so it can be restored later
    DEPTH saved-depth !
    DEPTH 0 ?DO
        saved-stack I CELLS + !
    LOOP
;

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

        actual-depth @ 0= UNLESS
            ."          "
            0 actual-depth @ 1- DO
                actual-stack I CELLS + @ .
                -1
            +LOOP

            CR
        THEN

        ."       want depth: " DEPTH . CR
        DEPTH 0= UNLESS
            ."          "
            DEPTH 1- 0 SWAP DO
                I PICK .
                -1
            +LOOP

            CR
        THEN

        restore-stack
        EXIT
    THEN
    \ Keep track of success state (so we can clear the stack)

    TRUE >R
    actual-stack        \ get base of actual stack array
    actual-depth @
    BEGIN
        ?DUP            \ loop until we consume the entire stack
    WHILE               \ ( addr depth -- )
        SWAP            \ ( depth addr )
        DUP >R          \ save stack addr for later
        @               \ get this item of the stack
        ROT             \ ( depth val TOS ) -- pull expected stack val up
        2DUP != IF      \ compare actual/expected
            failed!

            ."     value mismatch at offset " 2 PICK . CR
            ."       expected: " . CR
            ."       actual: " . CR

            R> R>          \ restore stack addr and true/false val ( addr res )
            DROP FALSE     \ drop previous result, replace with false
            >R >R          \ save addr and res ( R: res addr )
        ELSE
            2DROP
        THEN
        1-                 \ decrement stack depth counter
        R> CELL+           \ next addr in stack
        SWAP               \ ( addr depth )
    REPEAT
    DROP                   \ drop addr

    R> IF
        '.' EMIT
        passed!
    THEN

    restore-stack
;

: report-results
    #fail @ 0> IF Term.red ELSE Term.green THEN

    CR ." Finished with "
        #fail @ . ." failures out of "
        #pass @ . ." tests." CR

    Term.reset
;

