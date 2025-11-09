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

; Pre state:               Post state:
;   IP -> | &DUP |            WP ->  | &DUP | x0 -> &DUP
;         | &ADD |            IP ->  | &ADD |
;         | &DOT |                   | &DOT |
.macro NEXT
    ldr WP, [IP], #8
    ldr x0, [WP]
    br  x0
.endm

; "do-colon" or something. The forth threaded interpreter.
;
; Pre state:               Post state:
;   WP  -> | &DOCOL |            WP ->  | &DOCOL |
;          | &DUP   |            IP ->  | &DUP   |
;          | &DOT   |                   | &DOT   |
;
;   [return stack]              [return stack]
;   RSP -> |     |                     | IP  |
;                               RSP -> |     |
.align 8
docol:
	PUSH_RS IP
	add     IP, WP, #8
	NEXT

; Word layout
;
; struct word {
;   u64* prev_word;
;   struct {
;     int immediate: 1;
;     int reserved: 1; // can't remember right now
;   } flags;
;   u8 name_len;
;   u8[name_len] name;
;   byte[...] zero_padding; // align 8 bytes
;   u64[] ops;
; }
.macro DEFINE_WORD name, flags=0, prev_word, label
    .data
    .align 8
    .global  name_\label
    name_\label:
        .dword   \prev_word      ; point to the previously defined word
        .byte    \flags
        .byte    2f - 1f         ; calculate size of name
        1:
        .ascii   "\name"
        2:
        .align 8               ; add padding if necessary

    \label:
        .dword code_\label

    .text
    .global code_\label
    code_\label:
        .dword =docol
        ; filled in after macro call
.endm

; Creates a "variable" via a word which pushes an address to the stack, which
; can then be read or written to
.macro DEFINE_VAR name, flags=0, prev_word, label, initial=0
    DEFINE_WORD \name, \flags, \prev_word, \label
        LOAD_LABEL x0, var_\name
        PUSH_RS    x0
        NEXT

    .data
    .align 8
    var_\name: .dword \initial
.endm

; Creates a constant which pushes a literal value to the stack
.macro DEFINE_CONST name, flags=0, prev_word, label, value
    DEFINE_WORD \name, \flags, \prev_word, \label
        mov     x0, #\value
        PUSH_DS x0
        NEXT
.endm

;; Built-in variables
.set NIL, 0
DEFINE_VAR "STATE",  0, NIL,         STATE     ; Interpreter state: executing (0), or compiling (non-zero)
DEFINE_VAR "HERE",   0, name_STATE,  HERE      ; Pointer to next free byte of memory
DEFINE_VAR "LATEST", 0, name_HERE,   LATEST    ; Pointer to most recently defined word in dictionary
// DEFINE_VAR "BASE", 4, 0, name_S0, BASE, 10

;; Built-in constants
DEFINE_CONST "VERSION", 0, name_LATEST,  VERSION, 12300

;; Stack pointer "constants" which I can't use the macro for.

; Pointer to top of param stack
DEFINE_WORD "S0", 0, name_VERSION, S0
    LOAD_LABEL x0, DATA_STACK_TOP
    PUSH_DS    x0
    NEXT

; Pointer to top of return stack
DEFINE_WORD "R0", 0, name_S0, R0
    LOAD_LABEL x0, RETURN_STACK_TOP
    PUSH_DS    x0
    NEXT

; Set return stack pointer to top of stack
;
; TODO can define this in forth
DEFINE_WORD "RSP!", 0, name_R0, RSP
    POP_DS RSP
    NEXT

;; Built-in words
DEFINE_WORD "EXIT", 0, name_RSP, EXIT
    POP_RS IP
    NEXT

; A misnomer, QUIT does not exit the program, it is the top level loop, and
; initializes everything.
;
; Maybe "RESET" is a better name?
DEFINE_WORD "QUIT", 0, name_EXIT, QUIT
    .dword =R0           ; Push top of return stack
    .dword =RSP          ; Set return stack pointer
	.dword =INTERPRET    ; interpret next word
	; TODO [ BRANCH -16 ] (aka jump back 2 words)

; The REPL.
DEFINE_WORD "INTERPRET", 0, name_QUIT, INTERPRET
    bl _read_word
    ; TODO!
    NEXT

; (a -- )
DEFINE_WORD "DROP", 0, name_INTERPRET, DROP
	POP_DS x0
	NEXT

; (a -- a a)
DEFINE_WORD "DUP", 0, name_DROP, DUP
    ldr     x0, [DSP]
	PUSH_DS x0
	NEXT

; (a -- a)
DEFINE_WORD "1+", 0, name_DUP, INCR
    POP_DS  x0
    add     x0, x0, #1
    PUSH_RS x0
    NEXT

; (a -- )
DEFINE_WORD ".", 0, name_INCR, DOT
    POP_DS      x0
    PRINT_DEBUG "%lld\n", x0
    NEXT


_main:
    ; Initialize stack pointers
    LOAD_LABEL RSP, RETURN_STACK_TOP
    LOAD_LABEL DSP, DATA_STACK_TOP

    ; Initialize LATEST with pointer to itself
    ;
    ; FIXME: this assumes LATEST is the last word defined, not currently true
    LOAD_LABEL x2,  var_LATEST
    str        x2, [x2]

    bl print_prompt

    ; Initialize interpreter to point at QUIT
    ; TODO:
    ; LOAD_LABEL IP, code_QUIT
	; NEXT

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
    mov	    x0,  0   ; return value
    SYSCALL exit


.data
_word_buffer: .zero 32

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


.data
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
