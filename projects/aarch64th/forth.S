.text
.global _main

;;; Generic helpers
.macro PUSH r1, r2
    stp  \r1, \r2, [sp, -16]!
.endm

.macro PUSH4 r1, r2, r3, r4
    stp  \r1, \r2, [sp, -16]!
    stp  \r3, \r4, [sp, -16]!
.endm

.macro POP r1, r2
    ldp  \r1, \r2, [sp], #16
.endm

; Note reversed order - for symmetry with PUSH4
.macro POP4 r1, r2, r3, r4
    ldp  \r3, \r4, [sp], #16
    ldp  \r1, \r2, [sp], #16
.endm

; Preserve link and frame pointer
.macro PUSH_FRAME
    PUSH x29, x30
.endm

; Restore link and frame pointer
.macro POP_FRAME
    POP x29, x30
.endm

.macro BL_NO_CLOBBER label
    PUSH4 x0, x1, x2, x3              ; Save registers that could be clobbered
    bl    \label
    POP4  x0, x1, x2, x3              ; Discard args we pushed to the stack
.endm

.set DEBUG_PRINT_ENABLED, 1
.macro PRINT_DEBUG fmt, arg1=x0, arg2=x1, arg3=x2, arg4=x3
    .if DEBUG_PRINT_ENABLED == 1
        PRINT_FMT "\fmt", \arg1, \arg2, \arg3, \arg4
    .endif
.endm

.macro PRINT_FMT fmt, arg1=x0, arg2=x1, arg3=x2, arg4=x3
    .data
    .balign 8
    fmt_\@: .asciz "\fmt"

    .text
    PUSH_FRAME
    PUSH4      x0, x1, x2, x3              ; Save registers that could be clobbered

    PUSH4      \arg3, \arg4, \arg1, \arg2  ; Darwin keeps printf args on the stack
    ADR_LABEL  x0, fmt_\@                  ; Load format string
    bl         _printf                     ; Print

    POP4       \arg3, \arg4, \arg1, \arg2  ; Discard args we pushed to the stack
    POP4       x0, x1, x2, x3              ; Restore original registers
    POP_FRAME
.endm

; Align value in \reg to 8 byte boundary
.macro ALIGN8 reg
    add     \reg, \reg, #7
    and     \reg, \reg, #~7
.endm

;;; Sys/environment helpers

; Default file descriptors
.set FD_STDIN,  0
.set FD_STDOUT, 1
.set FD_STDERR, 2

; Darwin syscall numbers
;
; ref: https://github.com/opensource-apple/xnu/blob/master/bsd/kern/syscalls.master
.set SYS_NR_exit,  1    ; void exit(int rval)
.set SYS_NR_fork,  2    ; int fork(void)
.set SYS_NR_read,  3    ; ssize_t read(int fd, void *buf, size_t count)
.set SYS_NR_write, 4    ; ssize_t write(int fd, const void *buf, size_t count)
.set SYS_NR_open,  5    ; int open(const char *path, int flags, mode_t mode)
.set SYS_NR_close, 6    ; int close(int fd)

; Make a syscall. Params will be in x0, x1, etc.
.macro SYSCALL op
    mov x16, SYS_NR_\op
    svc 0x80
.endm

;;; Forth-y things
IP  .req x25  ; instruction pointer
WP  .req x26  ; word pointer
RSP .req x27  ; return stack pointer
DSP .req x28  ; data stack pointer

;; Word flags
.set FL_IMMED,  (1 << 0)
.set FL_HIDDEN, (1 << 1)

; Push \reg to return stack
.macro PUSH_RS reg
    str \reg, [RSP, #-8]!
.endm

; Pop from return stack into \reg
.macro POP_RS reg
    ldr \reg, [RSP], #8
.endm

; Push \reg to data stack
.macro PUSH_DS reg
    str \reg, [DSP, #-8]!
.endm

; Pop from data stack into \reg
.macro POP_DS reg
    ADR_LABEL \reg, DATA_STACK_TOP
    sub       \reg, DSP, \reg
    cbz       \reg, 1f
    b         2f
1:
    PRINT_FMT "ERR STACK UNDERFLOW\n"
    mov       \reg, xzr
    b         3f
2:
    ldr       \reg, [DSP], #8
3:
.endm

; Load address of label into \reg
.macro ADR_LABEL reg, label
    adrp \reg,       \label@page
    add  \reg, \reg, \label@pageoff
.endm

; Load value of memory at address pointed to by label into register
.macro LDR_LABEL reg, label
    ADR_LABEL \reg, \label
    ldr       \reg, [\reg]
.endm

; NEXT is the beating heart of the interpreter. It's a bit tricky.
;
; Imagine we have the following definitions
;
;   : DOUBLE DUP ADD ;
;   : QUAD   DOUBLE DOUBLE ;
;
; Then let's say we're executing "QUAD"
;
;   Before NEXT:
;      QUAD:             DOUBLE:        DOCOL asm:
;        DOCOL  <- IP      DOCOL          str IP, [RSP, #-8]!
;        DOUBLE            DUP            ...
;        DOUBLE            ADD            ret
;        EXIT              EXIT
;
;   After NEXT:
;      QUAD:             DOUBLE:         DOCOL asm:
;        DOCOL             DOCOL <- WP     str IP, [RSP, #-8]!  <- x0, pc
;        DOUBLE <- IP      DUP             ...
;        DOUBLE            ADD             ret
;        EXIT              EXIT
;
; This sets the stage for DOCOL (implemented in assembly), which saves the state
; of IP within QUAD (on return stack) and then advances it to the next word of
; DOCOL
.macro NEXT
    ldr WP, [IP], #8  ; set WP to addr of word to execute, move IP to next word
    ldr x0, [WP]      ; load the addr of codeword for next word (will be DOCOL or an
                      ; asm offset)
    br  x0            ; jump to the codeword
.endm

; "do-colon" or something. The forth threaded interpreter.
;
; Continuing the example from the definition of NEXT, here's our "before" state
;      QUAD:             DOUBLE:
;        DOCOL            DOCOL <- WP
;        DOUBLE <- IP     DUP
;        DOUBLE           ADD
;        EXIT             EXIT
;
; After DOCOL
;      [Return Stack]    QUAD:        DOUBLE:
;      [ &old_ip ]        DOCOL        DOCOL
;           +-----------> DOUBLE       DUP   <- IP
;                         DOUBLE       ADD
;                         EXIT         EXIT
.text
.balign 8
DOCOL:
    PUSH_RS IP           ; Save instruction pointer (will be popped by EXIT)
    add     IP, WP, #8   ; WP points at the word we're executing (DOCOL), we want the next one
    NEXT

.macro WORD_HEADER name, len, flags, label, prev, code
    .data
    .balign 8
    name_\label:
        .ifc "\prev", "NIL"      ; store link to previously defined word (or NIL for the first word)
            .dword 0
        .else
            .dword "name_\prev"
        .endif

        .byte  \flags            ; immediate, hidden, etc. flags
        .byte  \len              ; length of the name string
        .ascii "\name"           ; word name

    .balign 8                    ; pad to word boundary
    \label:
        .dword \code             ; point to execution entry point (native code or DOCOL)
.endm

; "Native" words, defined in assembly
.macro DEFINE_CODE name, len, flags=0, label, prev
    WORD_HEADER "\name", \len, \flags, \label, \prev, "code_\label"

    .text
    .balign 8
    code_\label:
        ; PRINT_DEBUG "(exec \name)\n"
        ; body filled in after macro call
.endm

; Words defined in terms of other Forth words.
.macro DEFINE_WORD name, len, flags=0, label, prev
    WORD_HEADER "\name", \len, \flags, \label, "\prev", DOCOL
    ; body filled in after macro call
.endm

; Creates a "variable" via a word which pushes an address to the stack, which
; can then be read or written to
.macro DEFINE_VAR name, len, flags=0, label, prev_word, initial=0
    .data
    .balign 8
    var_\label:
        .dword \initial

    DEFINE_CODE \name, \len, \flags, \label, \prev_word
        ADR_LABEL x0, var_\label
        PUSH_DS   x0
        NEXT
.endm

; Creates a constant which pushes a literal value to the stack
;
; TODO: support larger constant values
.macro DEFINE_CONST name, len, flags=0, label, prev_word, value
    DEFINE_CODE \name, \len, \flags, \label, \prev_word
        mov     x0, #\value
        PUSH_DS x0
        NEXT
.endm

; A constant pointing to address of something else
.macro DEFINE_CONST_REF name, len, flags=0, label, prev_word, value_label
    DEFINE_CODE \name, \len, \flags, \label, \prev_word
        ADR_LABEL  x0, \value_label
        PUSH_DS    x0
        NEXT
.endm

;; Built-in variables
DEFINE_VAR "STATE", 5, 0, STATE, NIL   ; Interpreter state: executing (0), or compiling (non-zero)
DEFINE_VAR "HERE",  4, 0, HERE,  STATE ; Pointer to next free byte of memory
// TODO: DEFINE_VAR "BASE", 4, 0, name_S0, BASE, 10

;; Built-in constants
DEFINE_CONST "VERSION", 7, 0, VERSION, HERE, 0
DEFINE_CONST "CELL",    4, 0, CELL_SZ, VERSION, 8            ; Cell/word size in bytes
DEFINE_CONST_REF "DOCOL", 5, 0, PUSH_DOCOL, CELL_SZ, DOCOL   ; Push addr of DOCOL to the stack
DEFINE_CONST_REF "SP0", 3, 0, SZ, PUSH_DOCOL, DATA_STACK_TOP ; Pointer to top of param stack
DEFINE_CONST_REF "RP0", 3, 0, RZ, SZ, RETURN_STACK_TOP       ; Pointer to top of return stack

;; Built-in words

; ( -- a)
DEFINE_CODE "RP@", 3, 0, RSPFETCH, RZ
    PUSH_DS RSP
    NEXT

; (a --)
DEFINE_CODE "RP!", 3, 0, RSPSTORE, RSPFETCH
    POP_DS RSP
    NEXT

; ( -- a)
DEFINE_CODE "SP@", 3, 0, DSPFETCH, RSPSTORE
    mov     x0, DSP
    PUSH_DS x0
    NEXT

; (a --)
DEFINE_CODE "SP!", 3, 0, DSPSTORE, DSPFETCH
    POP_DS x0
    mov    DSP, x0
    NEXT

; Store data in a specific memory address
;
; (data addr --)
DEFINE_CODE "!", 1, 0, STORE, DSPSTORE
    POP_DS x0        ; address to write
    POP_DS x1        ; data to write
    str    x1, [x0]  ; *addr = data
    NEXT

; Fetch data from specific address
;
; (addr -- data)
DEFINE_CODE "@", 1, 0, FETCH, STORE
    POP_DS  x0       ; address to read
    ldr     x1, [x0] ; data = *addr
    PUSH_DS x1       ; save it to the stack
    NEXT

; Store single byte at specific addr
;
; ( data addr -- )
DEFINE_CODE "C!", 2, 0, STOREBYTE, FETCH
    POP_DS x0        ; addr to write
    POP_DS x1        ; value to write
    strb   w1, [x0]  ; write the value
    NEXT

; Read single byte from specific addr
;
; ( addr -- data )
DEFINE_CODE "C@", 2, 0, FETCHBYTE, STOREBYTE
    POP_DS  x0       ; addr to read
    ldrb    w1, [x0] ; read byte
    PUSH_DS x1       ; push back to stack
    NEXT

; TODO: document
DEFINE_CODE "'", 1, 0, TICK, FETCHBYTE
    ldr     x0, [IP], #8    ; load addr of next word and skip
    PUSH_DS x0
    NEXT

; Pop a value off the return stack and jump to it
;
; RS: (a -- )
DEFINE_CODE "EXIT", 4, 0, EXIT, TICK
    POP_RS IP
    NEXT

; Offset instruction pointer by the given offset (as an immediate value)
;
; Pre state:               Post state:
;   WP  -> | &BRANCH |              | &BRANCH |
;   IP  -> | 8       |              | 8       |
;          | &...    |       IP ->  | &...    |
DEFINE_CODE "BRANCH", 6, 0, BRANCH, EXIT
    ldr    x0, [IP]    ; load the branch offset
    add    IP, IP, x0  ; update pointer to offset
    NEXT

; As with BRANCH, but with a conditional from the stack
DEFINE_CODE "0BRANCH", 7, 0, ZBRANCH, BRANCH
    POP_DS x0               ; load top of stack
    cbz    x0, code_BRANCH  ; if zero, run BRANCH
    add    IP, IP, 8        ; else, skip over the immediate offset
    NEXT

; Given a word address, return the address of the first executable code of the
; word
;
; >CFA = "to Code Field Address"
DEFINE_CODE ">CFA", 4, 0, TO_CFA, ZBRANCH
    POP_DS  x0              ; load top of stack
    bl      _to_cfa
    PUSH_DS x0              ; push code pointer onto stack
    NEXT

_to_cfa:
    add    x1, x0, #9
    ldrb   w2, [x1]            ; w2 = name length

    add    x0, x0, #10         ; point at first name byte
    add    x0, x0, x2          ; advance by name length
    ALIGN8 x0                  ; align to 8 byte boundary

    ret

; Create a dictionary header for a new word
DEFINE_CODE "CREATE", 6, 0, CREATE, TO_CFA
    bl _read_word

    LDR_LABEL x2, var_HERE   ; Current value of HERE
    ADR_LABEL x3, var_LATEST ; Address of LATEST
    ldr       x4, [x3]       ; Value of LATEST

    str       x4, [x2]       ; push previous word as link pointer
    str       x2, [x3]       ; set LATEST word to current word
    add       x2, x2, #8     ; skip past link pointer
    strb      wzr, [x2], #1  ; push flags byte (keep it empty)
    strb      w1, [x2], #1   ; push length byte

1: ; push bytes of name to dictionary header
    cbz x1, 2f               ; Jump forward if we've finished pushing all the bytes

    ldrb w3, [x0], #1        ; Next byte of name
    strb w3, [x2], #1        ; Store byte

    sub x1, x1, 1            ; Decrement remaining length of word
    b 1b                     ; Repeat

2: ; done pushing name, finish by aligning to 8 bytes
    ALIGN8 x2                ; align to 8 byte boundary

    ADR_LABEL x1, var_HERE
    str       x2, [x1]

    NEXT

; Effectively a GOTO. Pop a value off the stack and go to it
DEFINE_CODE "EXECUTE", 7, 0, EXECUTE, CREATE
    POP_DS x0
    mov    WP, x0            ; ensure we set current word so DOCOL works
    ldr    x1, [WP]          ; get codeword from word (DOCOL etc.)
    br     x1                ; jump to entry point
    ; we won't ever return here, caller will call NEXT


; The REPL!
DEFINE_CODE "INTERPRET", 9, 0, INTERPRET, EXECUTE
    IS_LITERAL .req x24      ; keep track of whether we're reading a word or an int

    bl   _read_word          ; read a single word from input
    mov  IS_LITERAL, #0      ; start off with assumption that we're reading a word

    PUSH x0, x1              ; save word + length
    bl   _lookup_word        ; try to find our word in the dictionary
    POP  x1, x2              ; restore word + length (to other registers)
    cbz  x0, 1f              ; if zero, not found

    ; Word is in the dictionary
    ldr  x2, [x0, #8]        ; load flags
    and  x2, x2, FL_IMMED    ; check if the word is immediate
    PUSH x2, xzr
    bl   _to_cfa             ; load code pointer from word
    POP  x2, x3
    cbnz x2, 4f              ; if immediate, execute now
    b    2f                  ; not immediate, have to compile

1: ; word not found in the dictionary, maybe a literal?
    mov  x0, x1              ; word string
    mov  x1, x2              ; word length
    mov  IS_LITERAL, #1      ; we're reading a literal
    bl   _read_number        ; try to parse a number
    cbnz x1, 6f              ; jump to error on failure

    mov       x2, x0         ; save literal number for later
    ADR_LABEL x0, LIT        ; so that we can first insert LIT

2: ; Check interpreter state (compiling or executing)
    LDR_LABEL x1, var_STATE  ; 0 is executing, 1 is compiling
    cbz       x1, 4f         ; if executing, run now (and return)

    BL_NO_CLOBBER _comma     ; compile the word (as given, or the inserted LIT)

    cbz IS_LITERAL, 3f       ; if it's not a literal, can already execute
    mov  x0, x2              ; for literals, we need to write the immediate value to memory
    BL_NO_CLOBBER _comma

3: ; Finish INTERPRET call
    NEXT

4: ; Execute function for immediate mode
    cbnz IS_LITERAL, 5f
    mov  WP, x0              ; ensure we set current word so DOCOL works
    ldr  x1, [WP]            ; get codeword from word (DOCOL etc.)
    br   x1                  ; jump to entry point

5: ; Execute literal (push it to the stack)
    PUSH_DS x2
    NEXT

6: ; Handle parse errors
    PRINT_DEBUG "TODO: Parse error, unknown word: '%s'\n", x0
    NEXT

DEFINE_CODE ",", 1, 0, COMMA, INTERPRET
    POP_DS x0
    bl     _comma
    NEXT

; Write word at x0 to memory pointed to by HERE, then advance HERE
;
; Clobbers: x1, x2
_comma:
    ADR_LABEL x1, var_HERE     ; x1 = &HERE
    ldr       x2, [x1]         ; x2 = *HERE
    str       x0, [x2], #8     ; *(*HERE) = x0
    str       x2, [x1]         ; HERE = (*HERE + 8)
    ret

; Switch to immediate mode (STATE = 0). Has to be configured as an IMMEDIATE
; word so that we can switch the state while in compile mode.
;
; ( -- )
DEFINE_CODE "[", 1, FL_IMMED, LBRACKET, COMMA
    ADR_LABEL x0, var_STATE  ; load state addr
    str       xzr, [x0]      ; write 0 to state
    NEXT

; Switch to compile mode. Doesn't need to be an IMMEDIATE word.
;
; TODO: why? fuzzy on this distinction
;
; ( -- )
DEFINE_CODE "]", 1, 0, RBRACKET, LBRACKET
    ADR_LABEL x0, var_STATE      ; load state addr
    mov       x1, #1             ;
    str       x1, [x0]           ; write 1 (= COMPILE) to state
    NEXT

DEFINE_CODE "IMMEDIATE", 9, FL_IMMED, IMMEDIATE, RBRACKET
    LDR_LABEL x0, var_LATEST     ; load latest word
    ldrb      w1, [x0, #8]       ; load flags byte
    eor       x1, x1, FL_IMMED   ; toggle immediate bit
    strb      w1, [x0, #8]       ; write the byte back
    NEXT

; Toggles the "hidden" bit of a given word
;
; ( addr -- )
DEFINE_CODE "HIDDEN", 6, 0, HIDDEN, IMMEDIATE
    POP_DS    x0                 ; word addr
    ldrb      w1, [x0, #8]       ; load flags byte
    eor       x1, x1, FL_HIDDEN  ; toggle hidden bit
    strb      w1, [x0, #8]       ; write the byte back
    NEXT

; Push the word immediately after this instruction (a literal value) to the
; stack.
;
; Note when this is evaluated, IP is already pointing at the right address, so
; we just need to load from it and then increment it
;
; ( -- a)
DEFINE_CODE "LIT", 3, 0, LIT, HIDDEN
    ldr     x0, [IP], #8  ; load immediate value
    PUSH_DS x0            ; push it to the stack
    NEXT

; Read the next word of input and push the string and length to the stack.
;
; ( -- addr len)
DEFINE_CODE "WORD", 4, 0, WORD, LIT
    bl      _read_word   ; Read a word of input
    PUSH_DS x0           ; Push the addr of the read word
    PUSH_DS x1           ; Push the length of the word
    NEXT

; Read a character of input and push it to the stack
;
; ( -- k)
DEFINE_CODE "KEY", 3, 0, KEY, WORD
    bl      _read_key
    PUSH_DS x0
    NEXT

; Try to look up a word's address in the dictionary. Returns 0 when not found
;
; (addr len -- addr?)
DEFINE_CODE "FIND", 4, 0, FIND, KEY
    POP_DS  x1           ; length of string
    POP_DS  x0           ; address of string
    bl      _lookup_word ;
    PUSH_DS x0           ; push addr (or nil)
    NEXT

; Try to parse a number from a string (given as addr + length)
;
; (a b -- c d)
DEFINE_CODE "NUMBER", 6, 0, NUMBER, FIND
    POP_DS x1            ; length of string
    POP_DS x0            ; address of string
    bl     _read_number
    PUSH_DS x0           ; parsed number
    PUSH_DS x1           ; error value
    NEXT

; Push a literal character (given as immediate value) onto the stack
;
; ( -- a)
DEFINE_CODE "CHAR", 4, 0, CHAR, NUMBER
    bl      _read_word   ; get the next word (x0: addr, x1: len)
    ldrb    w0, [x0]     ; read first character of the word
    PUSH_DS x0           ; push to stack
    NEXT

; Used to implement string words in forth (." and S"). Takes an immediate value
; for string length.
;
; ( -- addr len )
DEFINE_CODE "LITSTRING", 9, 0, LITSTRING, CHAR
    ldr     x0, [IP], #8 ; read length of string as immediate value
    PUSH_DS IP           ; push the addr of the string
    PUSH_DS x0           ; push the length
    add     IP, IP, x0   ; move IP to end of string
    ALIGN8  IP           ; align IP to cell boundary
    NEXT

; Print a string (all at once rather than per-char as per EMIT).
;
; TODO: implement in Forth
;
; ( addr len -- )
DEFINE_CODE "TELL", 4, 0, TELL, LITSTRING
    mov     x0, FD_STDOUT  ; param 1: stdout
    POP_DS  x2             ; param 3: length
    POP_DS  x1             ; param 2: string addr
    SYSCALL write
    NEXT

; (a -- )
DEFINE_CODE "DROP", 4, 0, DROP, TELL
    POP_DS x0
    NEXT

; (a -- a a)
DEFINE_CODE "DUP", 3, 0, DUP, DROP
    ldr     x0, [DSP]
    PUSH_DS x0
    NEXT

; (a b -- b a)
DEFINE_CODE "SWAP", 4, 0, SWAP, DUP
    ldp     x0, x1, [DSP]
    stp     x1, x0, [DSP]
    NEXT

; (a b -- a b a)
DEFINE_CODE "OVER", 4, 0, OVER, SWAP
    ldp     x0, x1, [DSP]
    PUSH_DS x1
    NEXT

; TODO: write this in Forth when we have BASE
;
; (a -- )
DEFINE_CODE ".", 1, 0, DOT, OVER
    POP_DS      x2                    ; value to print
    mov         x3, x2                ; copy it (we print it twice)

    ADR_LABEL   x0, _dot_buffer       ; string buffer to write to
    ADR_LABEL   x1, _dot_sprintf_fmt  ; string format
    PUSH        x2, x3                ; format args go on the stack
    bl          _sprintf              ; sprintf(&buffer, &fmt, ...args)
    mov         x2, x0                ; number of bytes written returned in x0
                                      ; (also third param to write())
    mov         x0, FD_STDOUT         ; param 1: stdout
    ADR_LABEL   x1, _dot_buffer       ; param 2: string addr
    SYSCALL     write
    add         sp, sp, #16           ; pop fmt args off the stack

    NEXT

.data
.balign 4
_dot_buffer: .zero 64
_dot_sprintf_fmt: .asciz "%d (0x%x)"
.text

; Print a single byte to output
;
; (a -- )
DEFINE_CODE "EMIT", 4, 0, EMIT, DOT
    mov     x0, FD_STDOUT  ; param 1: stdout
    mov     x1, DSP        ; param 2: string addr
    mov     x2, #1         ; param 3: length
    SYSCALL write
    POP_DS  x0
    NEXT

; (a b -- c)
DEFINE_CODE "+", 1, 0, ADD, EMIT
    POP_DS      x1
    POP_DS      x0
    add         x0, x0, x1
    PUSH_DS     x0
    NEXT

; (a b -- c)
DEFINE_CODE "-", 1, 0, SUB, ADD
    POP_DS      x1
    POP_DS      x0
    sub         x0, x0, x1
    PUSH_DS     x0
    NEXT

; (a b -- c)
DEFINE_CODE "*", 1, 0, MUL, SUB
    POP_DS      x1
    POP_DS      x0
    mul         x0, x0, x1
    PUSH_DS     x0
    NEXT

; (a b -- c)
DEFINE_CODE "/", 1, 0, DIV, MUL
    POP_DS      x1
    POP_DS      x0
    sdiv        x0, x0, x1
    PUSH_DS     x0
    NEXT

; Push 1 if equal, 0 else
;
; (a b -- c)
DEFINE_CODE "=", 1, 0, EQ, DIV
    POP_DS      x1
    POP_DS      x0
    cmp         x0, x1
    cset        x0, eq
    PUSH_DS     x0
    NEXT

; Push 1 if a < b, 0 else
;
; (a b -- c)
DEFINE_CODE "<", 1, 0, LT, EQ
    POP_DS      x1
    POP_DS      x0
    cmp         x0, x1
    cset        x0, lt
    PUSH_DS     x0
    NEXT

; (a b -- c)
DEFINE_CODE "AND", 3, 0, AND, LT
    POP_DS  x1
    POP_DS  x0
    and     x2, x0, x1
    PUSH_DS x2
    NEXT

; (a b -- c)
DEFINE_CODE "XOR", 3, 0, XOR, AND
    POP_DS  x1
    POP_DS  x0
    eor     x2, x0, x1
    PUSH_DS x2
    NEXT

; (a -- b)
DEFINE_CODE "INVERT", 6, 0, INVERT, XOR
    POP_DS  x0
    orn     x0, xzr, x0
    PUSH_DS x0
    NEXT

;; Debug utils

DEFINE_CODE ".stack", 6, 0, PRINT_STACK, INVERT
    mov       x0, DSP
    ADR_LABEL x1, DATA_STACK_TOP

    sub       x2, x1, x0
    lsr       x2, x2, 3

    PRINT_FMT "STACK [DSP: %p, size: %d]\n", x0, x2
1:
    cmp       x0, x1       ; have we reached top of stack?
    beq       2f           ; we're done
    ldr       x2, [x0]     ; load stack item
    mov       x3, x2

    PRINT_FMT "  %p (%d)\n", x2, x3

    add       x0, x0, #8   ; next word
    b         1b
2:
    PRINT_FMT "----\n\n"
    NEXT

DEFINE_CODE ".rstack", 7, 0, PRINT_RSTACK, PRINT_STACK
    mov       x0, RSP
    ADR_LABEL x1, RETURN_STACK_TOP

    sub       x2, x1, x0
    lsr       x2, x2, 3

    PRINT_FMT "RSTACK [RSP: %p, size: %d]\n", x0, x2
1:
    cmp       x0, x1       ; have we reached top of stack?
    beq       2f           ; we're done
    ldr       x2, [x0]     ; load stack item
    mov       x3, x2

    PRINT_FMT "  %p (%d)\n", x2, x3

    add       x0, x0, #8   ; next word
    b         1b
2:
    PRINT_FMT "----\n\n"
    NEXT

DEFINE_CODE ".dict", 5, 0, PRINT_DICT, PRINT_RSTACK
    bl print_dictionary
    NEXT

;; Words implemented in terms of other Forth words.

DEFINE_WORD ":", 1, 0, COLON, PRINT_DICT
    .dword CREATE                      ; Create a dictionary header for the word
    .dword LIT, DOCOL, COMMA           ; Append "DOCOL" prefix to the header
    .dword LATEST, FETCH, HIDDEN       ; Hide the current word while we define it
    .dword RBRACKET                    ; Switch to compile mode while reading the definition
    .dword EXIT                        ; Finish ":" execution

; A misnomer, QUIT does not exit the program, it is the top level loop, and
; initializes everything.
;
; Maybe "RESET" is a better name?
DEFINE_WORD "QUIT", 4, 0, QUIT, COLON
    .dword RZ         ; Push top of return stack
    .dword RSPSTORE   ; Set return stack pointer
    .dword INTERPRET  ; interpret next word
    .dword BRANCH     ; jump back...
    .dword (-8 * 2)   ; ... by 2 words

DEFINE_WORD ";", 1, FL_IMMED, SEMICOLON, QUIT
    .dword LIT, EXIT, COMMA            ; Append "EXIT" to current word definition
    .dword LATEST, FETCH, HIDDEN       ; Unhide the current word definition
    .dword LBRACKET                    ; Back to immediate mode
    .dword EXIT                        ; Finish ";" execution

DEFINE_WORD "HIDE", 4, 0, HIDE, SEMICOLON
    .dword WORD, FIND
    .dword HIDDEN
    .dword EXIT

; Pointer to most recently defined word in dictionary (to be convenient, we
; ensure it also starts out as the last defined word).
DEFINE_VAR "LATEST", 6, 0, LATEST, SEMICOLON

; Print the entire dictionary chain for debugging
; Iterates from LATEST back through the linked list
.text
.balign 8
print_dictionary:
    PUSH_FRAME
    PRINT_FMT "\n=== Dictionary ===\n"

    ADR_LABEL x3, DOCOL
    PRINT_DEBUG "docol:  %p\n", x3
    LDR_LABEL x3, var_LATEST  ; start with LATEST

1: ; loop through linked list
    cbz x3, 2f                ; if nil, we're done

    ; Get and print name length
    ldrb w4, [x3, #9]         ; load length byte (offset 9)

    ; Print name - need to copy to null-terminated buffer
    mov  x5, #0               ; counter
    ADR_LABEL x6, _word_buffer
    add  x7, x3, #10         ; point to start of name
3:
    cmp x5, x4               ; reached end of name?
    bge 4f
    ldrb w8, [x7, x5]       ; load character
    strb w8, [x6, x5]       ; store to buffer
    add  x5, x5, #1
    b 3b
4:
    strb wzr, [x6, x5]       ; null terminate

    PRINT_FMT "word: '%s'\n", x6
    PRINT_FMT "\taddr\t%p  ", x3, xzr

    ; Get and print codeword address
    mov  x0, x3
    bl   _to_cfa
    mov  x1, x0
    ldr  x2, [x0]              ; load actual codeword
    PRINT_FMT "\tCFA\t%p\t*CFA\t%p", x1, x2

    ; Move to next word in chain
    ldr x3, [x3]             ; load link pointer
    PRINT_FMT "\tnext\t%p\n", x3
    b 1b

2: ; done
    PRINT_FMT "==================\n\n"
    POP_FRAME
    ret

; The "source code" of our interpreter. We start with IP pointed here (it needs
; to be a memory address due to the double indirection of our interpreter).
.data
.balign 8
forth_entry_point:
    .dword QUIT
    ; the QUIT loop is never expected to terminate, so add some obvious marker
    ; data if we overrun it somehow
    .dword 0xfafafafafafafafa

.text
.balign 8
_main:
    ; Initialize stack pointers
    ADR_LABEL RSP, RETURN_STACK_TOP
    ADR_LABEL DSP, DATA_STACK_TOP

    ADR_LABEL x0, var_HERE            ; x0 = &var_HERE
    ADR_LABEL x1, USER_DATA_SEGMENT   ; x1 = &USER_DATA_SEGMENT
    str       x1, [x0]                ; *HERE = &USER_DATA_SEGMENT

    ADR_LABEL x0, var_LATEST          ; x0 = &var_LATEST
    ADR_LABEL x1, name_LATEST         ; x1 = &LATEST
    str       x1, [x0]                ; *var_LATEST = &LATEST

    ; bl print_prompt

    ; Initialize interpreter to point at QUIT
    ADR_LABEL IP, forth_entry_point
    NEXT

    ; We should never actually get here, the interpreter loop keeps going.
    mov     x0,  0   ; return value
    SYSCALL exit

print_prompt:
    ; TODO: refactor this
    PRINT_FMT "forth> "
    ret

; Jump to \label if \reg contains whitespace (\s\t\n\r)
;
; Clobbers x9
;
; \t\n\v\f\r = 09 0A 0B 0C 0D
.macro B.SPACE reg, label
    cmp \reg, ' '        ; space?
    beq \label
    sub x9, \reg, #0x09  ; temp: reg - 9
    cmp x9, #4           ; is it in range [0x09, 0x0D]?
    bls \label           ; branch if unsigned less than or equal
.endm

; Read the next forth word from stdin
;
; Returns
;   x0: word pointer
;   x1: word length
.text
.balign 8
_read_word:
    PUSH_FRAME
1: ; Search for first non-blank character.
    bl _read_key

    B.SPACE x0, 1b     ; read next if we saw a blank char

    ADR_LABEL x22, _word_buffer

2: ; start writing characters into the buffer (no bounds checks!)
    strb w0, [x22], #1 ; append the character to the buffer
    bl   _read_key     ; get the next key

    B.SPACE x0, 3f     ; check if we've hit the end of the word
    cbz     x0, 3f     ; is this the end of the file?
    b       2b         ; otherwise, keep iterating

3: ; compute length of the word and return
    ADR_LABEL x0, _word_buffer  ; load address of buffer
    sub       x1, x22, x0       ; find the length of the word

    strb      wzr, [x22]        ; add null byte so we can print more easily

    POP_FRAME
    ret

; Read next key from stdin, filling buffer if necessary.
;
; Return
;    x0: char (in low order byte of x0)
.balign 8
_read_key:
    PUSH_FRAME
1:
    LDR_LABEL x0, buffer_top  ; last valid position in input buffer
    ADR_LABEL x2, buffer_pos  ; addr of current position in input buffer
    ldr       x1, [x2]        ; current position in input buffer

    cmp x1, x0                ; if cur_pos >= top
    bge 2f                    ; we've exhausted input buffer, need to read

    ldrb w0, [x1], #1         ; read next byte from input buffer
    str  x1, [x2]             ; persist updated buffer position

    POP_FRAME
    ret

2: ; out of input, need to consume more from stdin
    mov       x0, FD_STDIN      ; param 1, stdin
    ADR_LABEL x1, input_buffer  ; param 2, destination
    mov       x2, BUFFER_SIZE   ; param 3, max read size

    SYSCALL read

    cmp xzr, x0 ; check if we got an EOF or error return
    bge 3f

    ADR_LABEL x1, input_buffer ; load pointer to start of buffer
    ADR_LABEL x2, buffer_top   ; addr of last valid position in input buffer
    ADR_LABEL x3, buffer_pos   ; addr of current position in input buffer
    add       x4, x1, x0       ; new top: input_buffer + bytes read

    str       x1, [x3]         ; reset position to start of buffer
    str       x4, [x2]         ; store new top

    b 1b

3: ; EOF (or error)
    PRINT_DEBUG "eof, exit\n"
    mov     x0, #-1  ; exit code
    SYSCALL exit

; How to parse a number in base 10?
;
; sign = 1
; ctr = 0
; if char[0] == '-':
;   sign = -1
;   ctr = 1
; acc = 0
; for (ctr in range(len))
;   tmp = char[ctr]
;   if tmp < '0' || tmp > '9':
;       throw
;   acc = acc * 10 + tmp
;  return acc * sign
;
; inputs:
;   x0: addr of input string
;   x1: length of input string
;
; outputs:
;   x0: parsed value
;   x1: error?
.balign 8
_read_number:
    cbz x1, 5f                   ; Check for empty string

    mov  x2, #1                  ; positive = 1
    mov  x3, #0                  ; ctr = 0

    ldrb w4, [x0]                ; load first char
    cmp  x4, '-'                 ; is it a negative number?
    bne  1f

    mov  x2, #0                  ; positive = 0
    mov  x3, #1                  ; ctr = 1

1:
    mov  x4, #0                  ; acc = 0

2:
    cmp x3, x1                   ; ctr >= len?
    bge 3f

    ldrb w5, [x0, x3]            ; tmp = ptr[ctr]

    ; Check if digit is in range '0' to '9'
    cmp x5, '0'
    blt 5f
    cmp x5, '9'
    bgt 5f

    sub x5, x5, '0'              ; Convert ASCII to digit value
    mov x6, #10
    mul x4, x4, x6               ; acc *= 10
    add x4, x4, x5               ; acc += digit
    add x3, x3, #1               ; ctr++
    b   2b

3:
    cbnz x2, 4f                   ; is the number positive?
    neg  x4, x4                   ; make negative

4:
    mov x0, x4                    ; return value
    mov x1, #0                    ; no error
    ret

5:
    mov x1, #1                    ; error flag
    ret

; Try to look up a word in the dictionary. Return its address, or 0 if not
; found.
;
;   input:
;     x0: addr of lookup string
;     x1: length of string
;   output:
;     x0: addr of word (or nil)
.balign 8
_lookup_word:
    PUSH_FRAME
    ADR_LABEL x2, var_LATEST  ; start with the last word defined and work backwards

1: ; loop through linked list
    ldr x2, [x2]              ; load next word in the list
    cbz x2, 3f                ; hit the nil pointer, exit

    ldrh w3, [x2, #8]         ; load flags (offset 8) and len (offset 9) bytes
    and  w4, w3, FL_HIDDEN    ; check if word is hidden (low byte)
    cbnz w4, 1b               ; word is hidden, skip

    lsr w3, w3, #8            ; shift length into low byte
    cmp w1, w3                ; check length before full string match
    bne 1b                    ; length differs, try next word

    mov x3, #0                ; char counter
    mov x4, x0                ; copy of input ptr that we can clobber
    add x5, x2, #10           ; copy of candidate ptr that we can clobber, pointing at word

2: ; length equal, loop string comparison
   ; TODO: optimized version w/ 64 bit load/compare

    ldrb w6, [x4], #1         ; load next search char
    ldrb w7, [x5], #1         ; load next candidate char
    cmp w6, w7                ; do characters match?
    bne 1b                    ; if not, skip
    add  x3, x3, #1           ; increment counter
    cmp  x3, x1               ; have we reached the end of the search string?
    bne  2b                   ; if not, keep looping

3: ; Either found a result or ran out of words to check
    mov x0, x2
    POP_FRAME
    ret

.data
.balign 8
fmt_prompt: .asciz "forth> "

.bss
.balign 8
DATA_STACK:       .zero (8 * 2048)
DATA_STACK_TOP:   .dword

RETURN_STACK:     .zero (8 * 2048)
RETURN_STACK_TOP: .dword

; TODO: allocate from system
.balign 8
USER_DATA_SEGMENT: .space 0x10000, 0xfafafafafafafafa

; Space to to store parsed words
_word_buffer:      .zero 32

.data
.balign 8
.set BUFFER_SIZE, 4096

buffer_pos: .dword input_buffer    ; Current position in buffer (initialized to start)
buffer_top: .dword _boot_file_end  ; End of data in buffer (initialized to end of the boot file)

; This buffer is initially loaded with a boot-up script containing Forth code,
; but after that's been consumed will be refilled with data read from stdin.
input_buffer:
    .incbin "./boot.f"                      ; Embed boot file as first thing to execute
_boot_file_end:

    ; Pad remaining space with zeros
    .set BOOT_SIZE, (_boot_file_end - input_buffer)
    .if BOOT_SIZE < BUFFER_SIZE
        .zero BUFFER_SIZE - (. - input_buffer)
    .endif
