VARIABLE stack-effect
    1024 CELLS ALLOT
VARIABLE stack-effect-ptr
    stack-effect stack-effect-ptr !

: GUARD{ ( -- )
    DEPTH stack-effect-ptr !
    CELL stack-effect-ptr +!
;

: }GUARD ( delta -- )
    DEPTH 1-
    CELL stack-effect-ptr -!
    stack-effect-ptr @
    -
    2DUP != IF
        cr
        ." [DEPTH GUARD] unexpected stack effect." cr
        ."      change:   " . cr
        ."      expected: " . cr
        cr
    ELSE
        2DROP
    THEN
;

: 0}GUARD ( -- ) 0 }GUARD ;

: word>prev      @ ;
: word>flags     CELL+ c@ ;
: word>name-len  CELL+ 1+ C@ ;
: word>name      DUP word>name-len SWAP CELL+ 2 + SWAP ;
: word>entry     word>name + ALIGNED ;

: printable? ( ch -- b ) BL '~' BETWEEN ;

: dump-ascii ( addr len -- )
    0 ?DO
        DUP C@
        DUP printable? UNLESS DROP '.' THEN EMIT
        1+
    LOOP
    DROP
;

\ Print out `cnt` bytes of memory starting at `addr`
: DUMP ( addr cnt -- )
    BASE @ >R HEX
    DUP >R

    0 ?DO
        I CELL MOD 0= IF
            DUP . SPACE
        THEN

        DUP C@ 2 u.r0 SPACE 1+

        I CELL MOD CELL 1- = IF
            SPACE
            DUP CELL- CELL dump-ascii CR
        THEN
    LOOP

    R> CELL MOD ?DUP IF
        SPACE
        CELL OVER - 3 * SPACES
        dump-ascii CR
    THEN

    R> BASE !
    CR
;


: find-word ( addr - addr-word? )
    LATEST @
    BEGIN
        ?DUP
    WHILE
        2DUP >cfa = IF
            NIP
            EXIT
        THEN

        word>prev
    REPEAT
    DROP

    0
;


: ? ( addr -- ) @ . ;

: see-prefix ( addr -- )
    4 SPACES
    '(' EMIT SPACE hex. ')' EMIT SPACE
;

: SEE ( -- )
    BL WORD
    2DUP FIND DUP UNLESS
        DROP
        ." No such word: " TELL cr
        EXIT
    THEN

    -ROT
    ." : " TELL

    DUP word>flags immediate-bit AND IF ."  IMMEDIATE " THEN
    CR

    ."     \ addr  " DUP hex. CR
    ."     \ prev  '" DUP word>prev word>name TELL
           ." ' [ " DUP @ hex. ']' EMIT CR
    CR

    DUP word>entry @ DOCOL = UNLESS
        ."   ( assembled word )" cr
        ';' EMIT CR
        DROP
        EXIT
    THEN

    word>entry
    BEGIN
        CELL+
        DUP @ $fafafafa
    != WHILE
        DUP see-prefix
        DUP @
        DUP find-word ?DUP IF
            word>name TELL
        ELSE
            ." ( unknown ) " DUP hex.
        THEN

        CR

        CASE
            ['] LIT OF
                CELL+
                DUP see-prefix
                3 SPACES '[' EMIT SPACE
                DUP @
                DUP printable? IF
                    DUP . ." ( '" EMIT ." ' )"
                ELSE
                    .
                THEN
                ." ] LITERAL"
                CR
            ENDOF

            ['] LITSTRING OF
                CELL+
                DUP see-prefix
                3 SPACES ." ( len ) "
                DUP @
                DUP >R . CR
                CELL+
                DUP see-prefix
                3 SPACES ." ( str ) '"
                DUP R@ TELL ." '" CR
                R> + ALIGNED
                CELL-
            ENDOF

            ['] 0BRANCH OF
                CELL+
                DUP see-prefix
                3 SPACES '[' EMIT SPACE
                DUP @ DUP . >R
                ." ] LITERAL    \ jump addr "
                DUP R> + hex. CR
            ENDOF

            ['] BRANCH OF
                CELL+
                DUP see-prefix
                3 SPACES '[' EMIT SPACE
                DUP @ DUP . >R
                ." ] LITERAL    \ jump addr "
                DUP R> + hex. CR
            ENDOF

            ( else )
        ENDCASE
    REPEAT
    DROP

    ';' EMIT CR
;
