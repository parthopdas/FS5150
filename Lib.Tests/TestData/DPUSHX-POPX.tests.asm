%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Macros used in the tests
;; 
%macro VerifyPushPop 1
    mov bx, sp
    mov cx, ss
    mov word [bx-2], 0
    mov dx, 0deadh
    mov %1, dx
    push %1
    mov dx, %1
    ASSERT_ZERO_CMP {dx, 0deadh}
    ASSERT_ZERO_CMP {word [bx-2], 0deadh}
    mov di, sp
    add di, 2
    ASSERT_ZERO_CMP {bx, di}
    mov si, ss
    ASSERT_ZERO_CMP {cx, si}
    mov dx, 0
    mov %1, dx
    pop %1
    mov dx, %1    
    ASSERT_ZERO_CMP {dx, 0deadh}
    mov di, sp
    ASSERT_ZERO_CMP {bx, di}
    mov si, ss
    ASSERT_ZERO_CMP {cx, si}
%endmacro

;;
;; Variables used in the tests
;;
WordVar1    dw  0ffffh

;;
;; Start of Tests
;;
_main:

; push reg16  15  1   push ax/pop reg16   12  1   pop cx
_FACT_
VerifyPushPop ax

; push mem16  24EA    2 to 4  push word ptr [bx]/pop mem16   25EA    2 to 4  pop word ptr [si1]
_FACT_
VerifyPushPop {word [WordVar1]}

; push segreg 14  1   push ds/pop segreg (not CS) 12  1   pop es
_FACT_
VerifyPushPop es

; popf/pushf
%define Mask_ODZC   0000110001000001b
_FACT_
UnsetFlags
stc
std
mov bl, 10000000b
shl bl, 1 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}
ASSERT_NOT_ZERO_TEST {ax, Mask_DF}
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

__END_TEST_SUITE__
