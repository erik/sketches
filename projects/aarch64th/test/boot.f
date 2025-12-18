REQUIRE ./test/tester.f

." boot.f"

0  CONSTANT 0S
-1 CONSTANT 1S

TESTING" test framework"
T{ -> }T
T{ 1 2 3 -> 1 2 3 }T

123 T{ -> }T \ stack outside the tests shouldn't affect tests
T{ saved-depth @ saved-stack @ -> 1 123 }T DROP \ Stack should be preserved after test
T{ 1 DuP dup DUP -> 1 1 1 1 }T \ check case insensitivity

TESTING" binary operations"
T{ 0 1 > -> FALSE }T
T{ 1 0 > -> TRUE }T
T{ 1 1 > -> FALSE }T

T{ 0 1 < -> TRUE }T
T{ 1 0 < -> FALSE }T
T{ 1 1 < -> FALSE }T

T{ 0 0 >= -> TRUE }T
T{ 0 1 >= -> FALSE }T
T{ 1 0 >= -> TRUE }T

T{ 0 0 <= -> TRUE }T
T{ 0 1 <= -> TRUE }T
T{ 1 0 <= -> FALSE }T

T{ 0S 0S AND -> 0S }T
T{ 0S 1S AND -> 0S }T
T{ 1S 0S AND -> 0S }T
T{ 1S 1S AND -> 1S }T

T{ 0S 0S OR -> 0S }T
T{ 0S 1S OR -> 1S }T
T{ 1S 0S OR -> 1S }T
T{ 1S 1S OR -> 1S }T

T{ 0S 0S XOR -> 0S }T
T{ 0S 1S XOR -> 1S }T
T{ 1S 0S XOR -> 1S }T
T{ 1S 1S XOR -> 0S }T

T{ 1 10 MIN -> 1 }T
T{ 1 -1 MIN -> -1 }T
T{ 1 10 MAX -> 10 }T
T{ 1 -1 MAX -> 1 }T

T{ 0 INVERT 1 AND -> 1 }T
T{ 1 INVERT 1 AND -> 0 }T
T{ 0 INVERT -> -1 }T
T{ -1 INVERT -> 0 }T

T{ 0 NEGATE   -> 0 }T
T{ 10 NEGATE  -> -10 }T
T{ -10 NEGATE -> 10 }T

TESTING" stack manipulation"
T{ 1 2 OVER    -> 1 2 1 }T
T{ 3 2 1 2 PICK -> 3 2 1 3 }T
T{ 1 2 3 ROT   -> 2 3 1 }T
T{ 1 2 3 -ROT  -> 3 1 2 }T
T{ 1 2 3 ROT -ROT -> 1 2 3 }T
T{ 1 2 NIP     -> 2 }T
T{ 1 2 2DUP    -> 1 2 1 2 }T
T{ 1 2 3 3DUP  -> 1 2 3 1 2 3 }T
T{ 1 2 TUCK    -> 2 1 2 }T
T{ 0 ?DUP      -> 0 }T
T{ 1 ?DUP      -> 1 1 }T
T{ : tt1 >R R> ; -> }T
T{ : tt2 >R R@ RDROP ; -> }T
T{ 123 tt1 -> 123 }T
T{ 123 tt2 -> 123 }T
T{ DEPTH DEPTH -> 0 1 }T

TESTING" misc words"
T{ 1 0 5 WITHIN -> TRUE }T
T{ 1 1 5 WITHIN -> TRUE }T
T{ 1 2 5 WITHIN -> FALSE }T
T{ 5 2 5 WITHIN -> FALSE }T
T{ 6 2 5 WITHIN -> FALSE }T
T{ ?interpreting ?compiling -> TRUE FALSE }T
T{ : tt ?interpreting [ ?compiling ] ; tt -> FALSE TRUE }T
T{ 0 ALIGNED          -> 0 }T
T{ CELL 1- ALIGNED    -> CELL }T
T{ CELL ALIGNED       -> CELL }T
T{ 2 CELLS 1- ALIGNED -> 2 CELLS }T

TESTING" ticks and tacks"
T{ : t1 123 ;
   : t2 IMMEDIATE ['] t1 ;
   t2 EXECUTE -> 123 }T
T{ : t3 ' t1 ; -> }T
T{ t3 -> s" t1" FIND >CFA }T

TESTING" characters"
: t1 [CHAR] XYZ ;
: t2 [CHAR] A ;
: t3 [ t2 ] LITERAL ;

T{ CHAR A -> 65 }T
T{ CHAR a -> 97 }T
T{ 'x' -> CHAR x }T
T{ '\n' -> $0a }T
T{ t1 t2 -> CHAR X CHAR A }T
T{ t3 -> CHAR A }T

TESTING" word"
: tt WORD SWAP C@ ;
T{ BL tt HELLO -> 5 CHAR H }T
T{ ')' tt HELLO) -> 5 CHAR H }T
T{ CHAR " tt GOODBYE" -> 7 CHAR G }T

TESTING" interpretation / compilation semantics"
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

TESTING" catch / throw"
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

TESTING" recurse"
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

TESTING" case"
: cs1
    CASE
        1 OF 111 ENDOF
        2 OF 222 ENDOF
        3 OF 333 ENDOF
        4 6 RANGEOF 444 ENDOF
        999 SWAP
    ENDCASE
;

T{ 1 cs1 -> 111 }T
T{ 2 cs1 -> 222 }T
T{ 3 cs1 -> 333 }T
T{ 4 cs1 -> 444 }T
T{ 5 cs1 -> 444 }T
T{ 9 cs1 -> 999 }T

TESTING" :noname"
VARIABLE nn1
VARIABLE nn2
T{ :NONAME 123 ; EXECUTE -> 123 }T
T{ :NONAME 1234 ; nn1 ! -> }T
T{ :NONAME 9876 ; nn2 ! -> }T
T{ nn1 @ EXECUTE -> 1234 }T
T{ nn2 @ EXECUTE -> 9876 }T

TESTING" source / >IN"
T{ 123456 DEPTH OVER 9 < 35 AND + 3 + >IN !
-> 123456 23456 3456 456 56 6 }T

: tt SOURCE >IN ! DROP ;
T{ tt 123 456
    -> }T

TESTING" create / DOES>"
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

TESTING" do loops"
T{ : loop1 DO I LOOP ;      -> }T
T{ : loop2 DO I -1 +LOOP ;  -> }T
T{ : loop3 ?DO I LOOP ;     -> }T

T{  4  1 loop1 ->  1 2 3 }T
T{  2 -1 loop1 -> -1 0 1 }T

T{  1 4 loop2 -> 4 3 2 1 }T
T{ -1 2 loop2 -> 2 1 0 -1 }T

T{  1  1 loop3 -> }T
T{  4  1 loop3 -> 1 2 3 }T
T{  2 -1 loop3 -> -1 0 1 }T

: tt
    0
    SWAP 0 DO
        I 1+ 0 DO
            I J + 3 = IF
                I UNLOOP
                I UNLOOP
                EXIT
            THEN
            1+
         LOOP
      LOOP
;
T{ 1 tt -> 1 }T
T{ 2 tt -> 3 }T
T{ 3 tt -> 4 1 2 }T

TESTING" number parsing"
T{ s" 123" 10 parse-uint -> 123 TRUE }T
T{ s" 456" 10 parse-uint -> 456 TRUE }T
T{ s" 789" 10 parse-uint -> 789 TRUE }T
T{ s" ABC" 16 parse-uint -> 2748 TRUE }T
T{ s" def" 16 parse-uint -> 3567 TRUE }T
T{ s" 90"  10 parse-uint -> 90 TRUE }T
T{ s" 10"  16 parse-uint -> 16 TRUE  }T
T{ s" ff"  16 parse-uint -> 255 TRUE }T
T{ s" FF"  16 parse-uint -> 255 TRUE }T
T{ s" 1F"  10 parse-uint -> 0 FALSE  }T
T{ s" 12(3)" 10 parse-uint -> 0 FALSE  }T
T{ s" 12.3"  10 parse-uint -> 0 FALSE  }T

T{ s" 97" >NUMBER   -> 97 TRUE }T
T{ s" 123" >NUMBER  -> 123 TRUE }T
T{ s" -16" >NUMBER  -> -16 TRUE }T
T{ s" $10" >NUMBER  -> 16 TRUE }T
T{ s" -$10" >NUMBER -> -16 TRUE }T
T{ s" #10" >NUMBER  -> 10 TRUE }T
T{ HEX s" 10" >NUMBER DECIMAL -> 16 TRUE }T

T{ s" 'A'" >NUMBER  -> 65 TRUE }T
T{ s" 'a'" >NUMBER  -> 97 TRUE }T
T{ s" '\n'" >NUMBER -> 10 TRUE }T
T{ s" '\''" >NUMBER -> 39 TRUE }T
T{ s" '\\'" >NUMBER -> 92 TRUE }T
T{ s" 'a" >NUMBER   NIP -> FALSE }T
T{ s" '" >NUMBER    NIP -> FALSE }T
T{ s" '\" >NUMBER   NIP -> FALSE }T
T{ s" '\x'" >NUMBER NIP -> FALSE }T
T{ s" '\n" >NUMBER  NIP -> FALSE }T

TESTING" streq?"
: s1 s" abc" ;
: s2 s" abd" ;
: s3 s" abcde" ;
: s4 s" xyz" ;

T{ 0 0 s1 streq? -> FALSE }T
T{ 0 0 0 0 streq? -> TRUE }T
T{ s" abcd" s" xyz" streq? -> FALSE }T
T{ s" abcd" s" abc" streq? -> FALSE }T
T{ s" abc" s" abc" streq? -> TRUE }T
T{ s1 s1 streq? -> TRUE }T
T{ s1 s2 streq? -> FALSE }T
T{ s1 s3 streq? -> FALSE }T
T{ s1 s4 streq? -> FALSE }T

TESTING" include / require"
T{ 1 INCLUDE ./test/data/_incr.f
   -> 2 }T

T{ 1 INCLUDE ./test/data/_incr.f
     INCLUDE ./test/data/_incr.f
   -> 3 }T

T{ 1 REQUIRE ./test/data/_incr.f
     REQUIRE ./test/data/_incr.f
   -> 2 }T
