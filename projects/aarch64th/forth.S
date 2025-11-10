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
    LOAD_LABEL x0, fmt_\@                  ; Load format string
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

.macro LOAD_LABEL reg, label
    adrp \reg,       \label@page
    add  \reg, \reg, \label@pageoff
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
.macro DEFINE_CODE name, flags=0, label, prev_word
    .data
    .balign 8
    name_\label:
        .dword   \prev_word      ; point to the previously defined word
        .byte    \flags
        .byte    2f - 1f         ; calculate size of name
        1:
        .ascii   "\name"
        2:
        .balign 8                ; add padding if necessary

    .balign 8
    \label:
        ; No DOCOL here, we want to jump to a native assembly implementation
        ; instead
        .dword code_\label

    .text
    .balign 4
    code_\label:
        PRINT_DEBUG "CODE \name\n"
        ; filled in after macro call
.endm

; Words defined in terms of other Forth words.
.macro DEFINE_WORD name, flags=0, label, prev_word
    .data
    .balign 8
    name_\label:
        .dword   \prev_word      ; point to the previously defined word
        .byte    \flags
        .byte    2f - 1f         ; calculate size of name
        1:
        .ascii   "\name"
        2:
        .balign 8                ; add padding if necessary

    .balign 8
    \label:
        .dword =docol
        ; rest is filled in after macro call
.endm

; Creates a "variable" via a word which pushes an address to the stack, which
; can then be read or written to
.macro DEFINE_VAR name, flags=0, label, prev_word, initial=0
    .data
    .balign 8
    var_\label:
        .dword \initial

    DEFINE_CODE \name, \flags, \label, \prev_word
        LOAD_LABEL x0, var_\label
        PUSH_DS    x0
        NEXT
.endm

; Creates a constant which pushes a literal value to the stack
;
; TODO: support larger constant values
.macro DEFINE_CONST name, flags=0, label, prev_word, value
    DEFINE_CODE \name, \flags, \label, \prev_word
        mov     x0, #\value
        PUSH_DS x0
        NEXT
.endm

;; Built-in variables
.set NIL, 0
DEFINE_VAR "STATE",  0, STATE,  NIL        ; Interpreter state: executing (0), or compiling (non-zero)
DEFINE_VAR "HERE",   0, HERE,   name_STATE ; Pointer to next free byte of memory
DEFINE_VAR "LATEST", 0, LATEST, name_HERE  ; Pointer to most recently defined word in dictionary
// DEFINE_VAR "BASE", 4, 0, name_S0, BASE, 10

;; Built-in constants
DEFINE_CONST "VERSION", 0, VERSION, name_LATEST, 12300

;; Stack pointer "constants" which I can't use the macro for.

; Pointer to top of param stack
DEFINE_CODE "S0", 0, SZ, name_VERSION
    LOAD_LABEL x0, DATA_STACK_TOP
    PUSH_DS    x0
    NEXT

; Pointer to top of return stack
DEFINE_CODE "R0", 0, RZ, name_SZ
    LOAD_LABEL x0, RETURN_STACK_TOP
    PUSH_DS    x0
    NEXT

; Set return stack pointer to top of stack
;
; TODO can define this in forth
DEFINE_CODE "RSP!", 0, RSPSTORE, name_RZ
    POP_DS RSP
    NEXT

;; Built-in words
DEFINE_CODE "EXIT", 0, EXIT, name_RSPSTORE
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
DEFINE_CODE "BRANCH", 0, BRANCH, name_EXIT
    ldr    x0, [IP]    ; load the branch offset
    add    IP, IP, x0  ; update pointer to offset
    NEXT

; As with BRANCH, but with a conditional from the stack
DEFINE_CODE "0BRANCH", 0, ZBRANCH, name_BRANCH
    POP_DS x0               ; load top of stack
    cbz    x0, code_BRANCH  ; if zero, run BRANCH
    add    IP, IP, 8        ; else, skip over the immediate offset
    NEXT

; The REPL!
DEFINE_CODE "INTERPRET", 0, INTERPRET, name_ZBRANCH
    bl _read_word  ; read a single word from input
    ; TODO!
    NEXT

; (a -- )
DEFINE_CODE "DROP", 0, DROP, name_INTERPRET
    POP_DS x0
    NEXT

; (a -- a a)
DEFINE_CODE "DUP", 0, DUP, name_DROP
    ldr     x0, [DSP]
    PUSH_DS x0
    NEXT

; (a -- a)
DEFINE_CODE "1+", 0, INCR, name_DUP
    POP_DS  x0
    add     x0, x0, #1
    PUSH_RS x0
    NEXT

; (a -- )
DEFINE_CODE ".", 0, DOT, name_INCR
    POP_DS      x0
    PRINT_DEBUG "%lld\n", x0
    NEXT

;; Words implemented in terms of other Forth words.

; A misnomer, QUIT does not exit the program, it is the top level loop, and
; initializes everything.
;
; Maybe "RESET" is a better name?
DEFINE_WORD "QUIT", 0, QUIT, name_DOT
    .dword RZ         ; Push top of return stack
    .dword RSPSTORE   ; Set return stack pointer
    .dword INTERPRET  ; interpret next word
    .dword BRANCH     ; jump back...
    .dword (-8 * 2)   ; ... by 2 words

.text
.balign 8
_main:
    ; Initialize stack pointers
    LOAD_LABEL RSP, RETURN_STACK_TOP
    LOAD_LABEL DSP, DATA_STACK_TOP

    ; Initialize LATEST with pointer to itself
    ;
    ; FIXME: this assumes LATEST is the last word defined, not currently true
    ; LOAD_LABEL x2,  var_LATEST
    ; str        x2, [x2]

    bl print_prompt

    ; Initialize interpreter to point at QUIT
    LOAD_LABEL IP, QUIT
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

    cmp x0, ' '
    beq 1b

    ; we've found one, start tracking size
    LOAD_LABEL x22, _word_buffer
    MOV        x23, #0

2: ; start writing characters into the buffer (no bounds checks!)
    strb w0, [x22], #1 ; append the character to the buffer
    add  x23, x23, #1
    bl  _read_key      ; get the next key
    cmp x0, ' '        ; check if we've reached the end of the word
    bne 2b             ; otherwise keep consuming

3: ; compute length of the word and return
    LOAD_LABEL x0, _word_buffer       ; load address of buffer
    sub        x1, x22, x0            ; find the length of the word

    PRINT_DEBUG "word: '%s', len: %d\n", x0, x1

    POP_FRAME
    ret


; Read next key from stdin, filling buffer if necessary.
;    x0: char (in low order byte of x0)
_read_key:
    PUSH_FRAME
1:
    LOAD_LABEL x20, _read_key_curr_pos
    LOAD_LABEL x21, _read_key_buff_top

    ldr x0, [x21] ; addr of top
    ldr x1, [x20] ; addr cur_pos

    cmp x1, x0  ; if cur_pos >= top
    bge 2f      ; we've exhausted input buffer, need to read

    ldrb w0, [x1], #1  ; read next byte from input buffer
    str  x1, [x20]     ; persist updated buffer position

    POP_FRAME
    ret

2: ; out of input, need to consume more from stdin
    mov        x0, FD_STDIN      ; param 1, stdin
    LOAD_LABEL x1, input_buffer  ; param 2, destination
    mov        x2, BUFFER_SIZE   ; param 3, max read size

    SYSCALL read

    cmp xzr, x0 ; check if we got an EOF or error return
    bge 3f

    LOAD_LABEL x1, input_buffer ; load pointer to start of buffer
    add        x2, x1, x0       ; new top: input_buffer + bytes read
    str        x1, [x20]        ; reset position to start of buffer
    str        x2, [x21]        ; store new top

    b 1b

3: ; EOF (or error)
    ;; TODO: on eof, should return the word we read until here (unless it has length zero)
    PRINT_DEBUG "hit eof, exit\n"
    mov     x0, -1  ; exit code
    SYSCALL exit

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
