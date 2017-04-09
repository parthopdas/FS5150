%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables used in the tests
;;
ByteVar1    db  0feh
WordVar1    dw  0fh

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
mov ah, 0ffh
inc ah 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov bl, 1
inc bl
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
UnsetFlags
mov bx, 0ffffh
inc bx 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov cx, 1
inc cx
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

; PF
_FACT_
UnsetFlags
mov al, 000000010b
inc al
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov dl, 000000001b
inc dl
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

_FACT_
UnsetFlags
mov cx, 8010h
inc cx
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov bx, 0f00fh
inc bx
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}
xxxxxx:
; AF
_FACT_
UnsetFlags
mov al, 0fh
inc al
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_AF}

_FACT_
SetFlags
mov dh, 0eh
inc dh
GetFlags
ASSERT_ZERO_TEST {ax, Mask_AF}

_FACT_
UnsetFlags
mov cx, 100fh
inc cx
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_AF}

_FACT_
SetFlags
mov bx, 1
inc bx
GetFlags
ASSERT_ZERO_TEST {ax, Mask_AF}

; ZF
_FACT_
UnsetFlags
mov dl, 0ffh
inc dl
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov ch, 0feh
inc ch
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

_FACT_
UnsetFlags
mov si, 0ffffh
inc si
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov di, 0fffeh
inc di
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

; SF
_FACT_
UnsetFlags
mov dl, 07fh
inc dl
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov ch, 1
inc ch
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

_FACT_
UnsetFlags
mov si, 07fffh
inc si
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov di, 1
inc di
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

; OF
_FACT_
UnsetFlags
mov bl, 01111111b
inc bl
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

; What is the test case for NoOverflow with bit 0 going into 7th bit and bit 1 coming out of it

_FACT_
SetFlags
mov dl, 10010110b 
inc dl
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov ah, 11111111b 
inc ah
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 0111111111111111b
inc bx
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

; What is the test case for NoOverflow with bit 0 going into 15th bit and bit 1 coming out of it

_FACT_
SetFlags
mov cx, 1001011000000000b 
inc cx
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 1111111111111111b
inc cx
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

;;
;; Instruction Forms
;; 
; inc reg8  3   2   inc ah
_FACT_
mov ah, 0
inc ah
ASSERT_ZERO_CMP {ah, 1}

; inc [mem8]    15EA    2 to 4  inc byte ptr [bx]
_FACT_
inc byte [ByteVar1]
ASSERT_ZERO_CMP {byte [ByteVar1], 0ffh}

; inc reg16 2   1   inc si
_FACT_
mov si, 0ffffh
inc si
ASSERT_ZERO_CMP {si, 0}

; inc [mem16]   23EA    2 to 4  inc [WordVar]
_FACT_
inc word [WordVar1]
; TODO: This is the test for dref16 Change to 0ffh and 100h to repro.
ASSERT_ZERO_CMP {word [WordVar1], 010h}

__END_TEST_SUITE__
