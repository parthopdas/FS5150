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
; - Additional tests
BITS 16
ORG 0x0

_OneTestFailed:
hlt

_AllTestPassed:
hlt

; ADD Tests TODO
; - 8 bit and 16 bit
; - Variations
; - Flags tests
; - Macros

;; 8-Bit
; Test #1
mov al, 0x1
add al, 0x2
cmp al, 0x3
mov ax, __LINE__
jnz _OneTestFailed

;; 16-Bit
; Test #2
mov ax, 0x1
add ax, 0x2
cmp ax, 0x3
mov ax, __LINE__
jnz _OneTestFailed

jmp _AllTestPassed