%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables use in the tests
;;
bxsi        db  80h
ByteVar     db  0fh
WordVar     dw  08000h
WordVar1    dw  0ffffh

;;
;; Start of Tests
;;
_main:

;;
;; Flags Tests
;; 
; CF
_FACT_
UnsetFlags
mov ah, 80h
shl ah, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov bl, 7fh
shl bl, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

_FACT_
UnsetFlags
mov bx, 0ffffh
shl bx, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov cx, 4000h
shl cx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

; PF
_FACT_
UnsetFlags
mov al, 11111110b
shl al, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov dl, 01111111b
shl dl, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

_FACT_
UnsetFlags
mov cx, 1110000010000000b
shl cx, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov bx, 1100000011000000b
shl bx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

; ZF
_FACT_
UnsetFlags
mov dl, 80h
shl dl, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov ch, 1
shl ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

_FACT_
UnsetFlags
mov si, 8000h
shl si, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov di, 100h
shl di, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

; SF
_FACT_
UnsetFlags
mov dl, 40h
shl dl, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov ch, 80h
shl ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

_FACT_
UnsetFlags
mov si, 07000h
shl si, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov di, 03fffh
shl di, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

; OF
_FACT_
UnsetFlags
mov bl, 10000000b
shl bl, 1 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bh, 01000000b
shl bh, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov dl, 00000000b 
shl dl, 1 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov ah, 11000000b 
shl ah, 1 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 1001011000000000b
shl bx, 1 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 0111011000000000b
shl bx, 1 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 1101011000000000b 
shl cx, 1 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 0001011000000000b 
shl cx, 1 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

;;
;; Instruction Forms
;; 
; shl reg8,1  2   2   shl dl,1
_FACT_
mov dl, 2
shl dl, 1
ASSERT_ZERO_CMP {dl, 4}

; shl [mem8],1    15EA    2   to 4 shl byte ptr [bxsi],1
_FACT_
shl byte [bxsi], 1
ASSERT_ZERO_CMP {byte [bxsi], 0}

; shl reg16,1 2   2   shl cx,1
_FACT_
mov cx, 80h
shl cx, 1
ASSERT_ZERO_CMP {cx, 0100h}

; shl [mem16],1   23EA    2 to 4  shl word ptr [di],1
_FACT_
mov di, WordVar
shl word [di], 1
ASSERT_ZERO_CMP {word [di], 0}

; shl reg8,cl 8(4*CL) 2   shl al,cl
_FACT_
mov al, 0ffh
mov cl, 8
shl al, cl
ASSERT_ZERO_CMP {al, 0}

; shl [mem8],cl   20EA(4*CL)  2 to 4  shl [ByteVar],cl
_FACT_
mov cl, 4
shl byte [ByteVar], cl
ASSERT_ZERO_CMP {byte [ByteVar], 0f0h}

; shl reg16,cl    8(4*CL) 2   shl bp,cl
_FACT_
mov si, 0ffffh
mov cl, 10h
shl si, cl
ASSERT_ZERO_CMP {si, 0}

; shl [mem16],cl  28EA(4*CL)  2 to 4  shl [WordVar1],cl
_FACT_
mov cl, 8
shl word [WordVar1], cl
ASSERT_ZERO_CMP {word [WordVar1], 0ff00h}

__END_TEST_SUITE__
