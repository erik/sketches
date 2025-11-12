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

.macro PRINT_DEBUG fmt, arg1=x0, arg2=x1, arg3=x2, arg4=x3
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
    ldr \reg, [DSP], #8
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
;   : QUAD DOUBLE DOUBLE ;
;
; Then let's say we're executing "QUAD"
;
;   Before NEXT:
;      QUAD:             DOUBLE:
;        DOCOL  <- IP     DOCOL
;        DOUBLE           DUP
;        DOUBLE           ADD
;        EXIT             EXIT
;
;   Before NEXT:
;      QUAD:             DOUBLE:
;        DOCOL            DOCOL <- WP
;        DOUBLE <- IP     DUP
;        DOUBLE           ADD
;        EXIT             EXIT
;
; This sets the stage for DOCOL, which saves the state of IP within QUAD (on
; return stack) and then advances it to the next word of DOCOL
.macro NEXT
    ldr x0, [IP], #8  ; Read addr of word at instruction pointer
    ldr WP, [x0]      ; WP now points to entrypoint of next word
    br  WP            ; Start executing next word
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
.balign 4
.global docol
docol:
    PUSH_RS IP           ; Save instruction pointer (will be restored by EXIT)
    add     IP, WP, #8   ; WP points at the word we're executing we want the next one
    NEXT

; "Native" words, defined in assembly
.macro DEFINE_CODE name, len, flags=0, label, prev
    .data
    .balign 8
    name_\label:
        .dword   name_\prev ; point to the previously defined word
        .byte    \flags
        .byte    \len       ; TODO: can this be computed somehow??
        .asciz   "\name"
        .balign 8           ; add padding if necessary

    .balign 8
    \label:
        ; No DOCOL here, we want to jump to a native assembly implementation
        ; instead
        .dword code_\label

    .text
    .balign 4
    code_\label:
        PRINT_DEBUG "(exec \name)\n"
        ; filled in after macro call
.endm

; Words defined in terms of other Forth words.
.macro DEFINE_WORD name, len, flags=0, label, prev_word
    .data
    .balign 8
    name_\label:
        .dword   name_\prev_word ; point to the previously defined word
        .byte    \flags
        .byte    \len
        .asciz   "\name"
        .balign 8                ; add padding if necessary

    .balign 8
    \label:
        .dword =docol
        ; rest is filled in after macro call
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
.set NIL, 0
DEFINE_VAR "STATE", 5, 0, STATE, NIL   ; Interpreter state: executing (0), or compiling (non-zero)
DEFINE_VAR "HERE",  4, 0, HERE,  STATE ; Pointer to next free byte of memory
// DEFINE_VAR "BASE", 4, 0, name_S0, BASE, 10

;; Built-in constants
DEFINE_CONST "VERSION", 7, 0, VERSION, HERE, 0
DEFINE_CONST_REF "DOCOL", 5, 0, DOCOL, VERSION, docol   ; Magic sauce
DEFINE_CONST_REF "S0", 2, 0, SZ, DOCOL, DATA_STACK_TOP  ; Pointer to top of param stack
DEFINE_CONST_REF "R0", 2, 0, RZ, SZ, RETURN_STACK_TOP   ; Pointer to top of return stack

;; Built-in words

; Set return stack pointer to top of stack
DEFINE_CODE "RSP!", 4, 0, RSPSTORE, RZ
    POP_DS RSP
    NEXT

DEFINE_CODE "EXIT", 4, 0, EXIT, RSPSTORE
    POP_RS IP
    NEXT

; Offset instruction pointer by the given offset (as an immediate value)
;
; TODO: not sure if this is right?
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

; hack, but define a separate label so that we can call this as a fn as well as define a Forth word
_to_cfa:
    ldrb   w1, [x0, #9]     ; load length of name
    add    x1, x1, #(7 + 9) ; add 7 for alignment + 9 for offset of length byte
    and    x1, x1, #~7      ; clear lower 3 bits (align to 8)
    add    x0, x0, x1       ; skip past name + padding
    ret

; The REPL!
DEFINE_CODE "INTERPRET", 9, 0, INTERPRET, TO_CFA
    IS_LITERAL .req x24 ; keep track of whether we're reading a word or an int

    bl _read_word       ; read a single word from input
    mov IS_LITERAL, #0  ; start off with assumption that we're reading a word
    bl _lookup_word     ; try to find our word in the dictionary
    cbz x0, 1f          ; if zero, not found

    ; Word is in the dictionary
    ldr  x2, [x0, #8]     ; load flags
    and  x2, x2, FL_IMMED ; check if the word is immediate
    bl   _to_cfa          ; load code pointer from word
    cbnz x2, 4f           ; if immediate, execute now
    b    2f               ; not immediate, have to compile

1: ; word not found in the dictionary, maybe a literal?
    mov IS_LITERAL, #1
    PRINT_DEBUG "TODO: parse number\n"
    ; todo: try parse number
    ; todo: if err, throw parse error
    ; todo: else: compile as a reference to LIT
    ;

2: ; Check interpreter state (compiling or executing)
    LDR_LABEL x1, var_STATE  ; 0 is executing, 1 is compiling
    cbz       x1, 4f         ; if executing, run now (and return)

    PUSH4 x0, x1, x2, x3
    bl    _comma
    POP4  x0, x1, x2, x3

    ; todo: if literal, COMMA again

3: ; Finish INTERPRET call
    NEXT

4: ; Execute function for immediate mode
    cbnz IS_LITERAL, 5f
    ldr  x1, [x0]
    br   x1

5: ; Execute literal (push it to the stack)
    PUSH_DS x0
    NEXT

6: ; Handle parse errors
    PRINT_DEBUG "TODO: Parse error\n"
    NEXT

DEFINE_CODE ",", 1, 0, COMMA, INTERPRET
    POP_DS x0
    bl     _comma
    NEXT

; TODO: document
_comma:
    ADR_LABEL  x1, var_HERE
    ldr        x2, [x1]
    str        x0, [x2], #8
    str        x2, [x1]
    ret

; (a -- )
DEFINE_CODE "DROP", 4, 0, DROP, COMMA
    POP_DS x0
    NEXT

; (a -- a a)
DEFINE_CODE "DUP", 3, 0, DUP, DROP
    ldr     x0, [DSP]
    PUSH_DS x0
    NEXT

; (a -- a)
DEFINE_CODE "1+", 2, 0, INCR, DUP
    POP_DS  x0
    add     x0, x0, #1
    PUSH_RS x0
    NEXT

; (a -- )
DEFINE_CODE ".", 1, 0, DOT, INCR
    POP_DS      x0
    PRINT_DEBUG "%p\n", x0
    NEXT

;; Words implemented in terms of other Forth words.

; A misnomer, QUIT does not exit the program, it is the top level loop, and
; initializes everything.
;
; Maybe "RESET" is a better name?
DEFINE_WORD "QUIT", 4, 0, QUIT, DOT
    .dword RZ         ; Push top of return stack
    .dword RSPSTORE   ; Set return stack pointer
    .dword INTERPRET  ; interpret next word
    .dword BRANCH     ; jump back...
    .dword (-8 * 2)   ; ... by 2 words


; Pointer to most recently defined word in dictionary (to be convenient, we
; ensure it also starts out as the last defined word).
DEFINE_VAR "LATEST", 6, 0, LATEST, QUIT

.text
.balign 8
_main:
    ; Initialize stack pointers
    ADR_LABEL RSP, RETURN_STACK_TOP
    ADR_LABEL DSP, DATA_STACK_TOP

    ADR_LABEL x1, var_HERE
    ADR_LABEL x2, USER_DATA_SEGMENT
    str       x2, [x1]

    ADR_LABEL x2, var_LATEST
    ADR_LABEL x3, name_LATEST
    str       x3, [x2]        ; Initialize LATEST with pointer to last defined word (itself)

    bl print_prompt

    ; Initialize interpreter to point at QUIT
    ADR_LABEL IP, QUIT
    NEXT

    PRINT_DEBUG ">> exit\n\n\n"
    b   exit

print_value:
    ; TODO: refactor this
    PRINT_DEBUG "value: %lld\n", x0
    ret

print_prompt:
    ; TODO: refactor this
    PRINT_DEBUG "forth> ", x0
    ret

exit:
    mov     x0,  0   ; return value
    SYSCALL exit

; Read the next forth word from stdin
;   x0: word pointer
;   x1: word length
.text
_read_word:
    PUSH_FRAME
1: ; Search for first non-blank character.
    bl _read_key

    cmp x0, ' ' ; did we find a space?
    beq 1b      ; if so, read the next char

    ADR_LABEL x22, _word_buffer

2: ; start writing characters into the buffer (no bounds checks!)
    strb w0, [x22], #1 ; append the character to the buffer
    bl  _read_key      ; get the next key

    cmp x0, ' '        ; is this the end of the word?
    beq 3f             ; finish word
    cbz x0, 3f         ; is this the end of the file?
    b   2b             ; otherwise, keep iterating

3: ; compute length of the word and return
    ADR_LABEL x0, _word_buffer  ; load address of buffer
    sub       x1, x22, x0       ; find the length of the word

    POP_FRAME
    ret

; Read next key from stdin, filling buffer if necessary.
;    x0: char (in low order byte of x0)
_read_key:
    PUSH_FRAME
1:
    ADR_LABEL x20, _read_key_curr_pos
    ADR_LABEL x21, _read_key_buff_top

    ldr x0, [x21]      ; addr of top
    ldr x1, [x20]      ; addr cur_pos

    cmp x1, x0         ; if cur_pos >= top
    bge 2f             ; we've exhausted input buffer, need to read

    ldrb w0, [x1], #1  ; read next byte from input buffer
    str  x1, [x20]     ; persist updated buffer position

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
    add       x2, x1, x0       ; new top: input_buffer + bytes read
    str       x1, [x20]        ; reset position to start of buffer
    str       x2, [x21]        ; store new top

    b 1b

3: ; EOF (or error)
    PRINT_DEBUG "eof, exit\n"
    mov     x0, -1  ; exit code
    SYSCALL exit

; Try to look up a word in the dictionary. Return its address, or 0 if not
; found.
;
;   input:
;     x0: addr of lookup string
;     x1: length of string
;   output:
;     x0: addr of word (or nil)
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

.bss
RETURN_STACK:     .zero (8 * 2048)
RETURN_STACK_TOP: .dword
DATA_STACK:       .zero (8 * 2048)
DATA_STACK_TOP:   .dword
; Space to to store parsed words
_word_buffer:     .zero 32

.data
.balign 8
fmt_prompt: .asciz "forth> "

; Current place in input buffer (next character to read).
.align 8
curr:
_read_key_curr_pos: .dword 0x15151515

; Last valid data in input buffer + 1.
.align 8
top:
_read_key_buff_top: .dword 0x15151515

.align 8
.set BUFFER_SIZE, 4096
input_buffer: .zero BUFFER_SIZE

; fixme: whack!!
name_NIL: .space 0

; TODO: allocate from system
.data
USER_DATA_SEGMENT:
    .zero 0xFFFF
