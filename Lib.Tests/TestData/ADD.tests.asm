; Test Framework TODO:
; - Quit on first failure
; - All should pass
; - Test file should be testable with an existing emulator
; - All supporting instructions should have unit tests
;
; - nasm -f bin inctest.asm -o inctest.com
;
; Steps:
; - Two Tests
; - Test Tests
; - xunit test
; - YoLo
; - Additional tests
BITS 16
ORG 0x100

jmp _main

NumberOfTests   dw  0

_OneTestFailed:
hlt

_AllTestPassed:
hlt

_main:
; ADD Tests TODO
; - 8 bit and 16 bit
; - Variations
; - Flags tests
; - Macros

;; 8-Bit
; Test #1
inc word [NumberOfTests]
mov al, 0x1
add al, 0x2
cmp al, 0x3
jnz _OneTestFailed

;; 16-Bit
; Test #2
inc word [NumberOfTests]
mov ax, 0x1
add ax, 0x2
cmp ax, 0x3
jnz _OneTestFailed

jmp _AllTestPassed
