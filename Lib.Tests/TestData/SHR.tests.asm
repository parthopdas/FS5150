%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables use in the tests
;;
ByteVar     db  1
ByteVarbx   db  0ffh
WordVar     dw  1
WordVarsi   dw  0ffffh

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
mov ah, 1
shr ah, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov bl, 0feh
shr bl, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

_FACT_
UnsetFlags
mov bx, 0ffffh
shr bx, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov cx, 2
shr cx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

; PF
_FACT_
UnsetFlags
mov al, 01111111b
shr al, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov dl, 11111110b
shr dl, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

_FACT_
UnsetFlags
mov cx, 0000000100000011b
shr cx, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov bx, 0000001100000111b
shr bx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

; ZF
_FACT_
UnsetFlags
mov dl, 1
shr dl, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov ch, 80h
shr ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

_FACT_
UnsetFlags
mov si, 1
shr si, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov di, 80h
shr di, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

; SF
;;
;; Cannot set SF for SHR ops
;;

_FACT_
SetFlags
mov ch, 80h
shr ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

;;
;; Cannot set SF for SHR ops
;;

_FACT_
SetFlags
mov di, 0ffffh
shr di, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

; OF
; For SHR, OF is set to the high-order bit of the original operand.
_FACT_
UnsetFlags
mov bl, 10010110b
shr bl, 1 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov dl, 00010110b 
shr dl, 1 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 1001011000000000b
shr bx, 1 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 0101011000000000b 
shr cx, 1 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

;;
;; Instruction Forms
;; 
; shr reg8,1  2   2   shr al,1
_FACT_
mov al, 2
shr al, 1
ASSERT_ZERO_CMP {al, 1}

; shr [mem8],1    15EA    2 to 4  shr [ByteVar],1
_FACT_
shr byte [ByteVar], 1
ASSERT_ZERO_CMP {byte [ByteVar], 0}

; shr reg16,1 2   2   shr bx,1
_FACT_
mov bx, 2
shr bx, 1
ASSERT_ZERO_CMP {bx, 1}

; shr [mem16],1   23EA    2 to 4  shr word ptr [si],1
_FACT_
mov si, WordVar
shr word [si], 1
ASSERT_ZERO_CMP {word [si], 0}

; shr reg8,cl 8(4*CL) 2   shr dl,cl
_FACT_
mov dl, 0ffh
mov cl, 8
shr dl, cl
ASSERT_ZERO_CMP {dl, 0}

; shr [mem8],cl   20EA(4*CL)  2 to 4  shr [ByteVarbx],cl
_FACT_
mov cl, 4
shr byte [ByteVarbx], cl
ASSERT_ZERO_CMP {byte [ByteVarbx], 0fh}

; shr reg16,cl    8(4*CL) 2   shr si,cl
_FACT_
mov si, 0ffffh
mov cl, 10h
shr si, cl
ASSERT_ZERO_CMP {si, 0}

; shr [mem16],cl  28EA(4*CL)  2 to 4  shr [WordVarsi],cl
_FACT_
mov cl, 8
shr word [WordVarsi], cl
ASSERT_ZERO_CMP {word [WordVarsi], 0ffh}

__END_TEST_SUITE__
