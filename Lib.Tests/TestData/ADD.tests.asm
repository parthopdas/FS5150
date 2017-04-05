; ADD Tests TODO
; - 8 bit and 16 bit
; - Variations from MA ZofALP
; - Flags tests

BITS 16
ORG 0x100

jmp _main

NumberOfTests   dw  0

_OneTestFailed:
hlt

_AllTestPassed:
hlt

_main:

inc word [NumberOfTests]
mov al, 0x1
add al, 0x2
cmp al, 0x3
jnz _OneTestFailed

inc word [NumberOfTests]
mov ax, 0x1
add ax, 0x2
cmp ax, 0x3
jnz _OneTestFailed

jmp _AllTestPassed
