%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables use in the tests
;;
ByteVar     db  2
si1         db  1
WordVar     dw  4
dibp        dw  3
ByteVar2    db  5
WordVar2    dw  9
WordVar3    dw  10

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
sub ah, 2
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov bl, 1
sub bl, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

_FACT_
UnsetFlags
mov bx, 1
sub bx, 2
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov cx, 1
sub cx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

; PF
_FACT_
UnsetFlags
mov al, 00000000b
sub al, 00000000b
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov dl, 10000001b
sub dl, 00000001b
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

_FACT_
UnsetFlags
mov cx, 0
sub cx, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov bx, 8003h
sub bx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

; AF
_FACT_
UnsetFlags
mov al, 00000111b
sub al, -1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_AF}

_FACT_
SetFlags
mov dh, 00000111b
sub dh, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_AF}

_FACT_
UnsetFlags
mov cx, 086h
sub cx, -1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_AF}

_FACT_
SetFlags
mov bx, 0ffffh
sub bx, 0fh
GetFlags
ASSERT_ZERO_TEST {ax, Mask_AF}

; ZF
_FACT_
UnsetFlags
mov dl, 1
sub dl, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov ch, 1
sub ch, 2
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

_FACT_
UnsetFlags
mov si, -1
sub si, strict word 0ffffh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov di, -1
sub di, -2
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

; SF
_FACT_
UnsetFlags
mov dl, 0
sub dl, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov ch, 1
sub ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

_FACT_
UnsetFlags
mov si, 08001h
sub si, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov di, 08002h
sub di, 3
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

; OF
_FACT_
UnsetFlags
mov bl, 10010110b
sub bl, 01011101b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bh, 00110110b
sub bh, 10011101b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov dl, 10010110b 
sub dl, 10011100b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov ah, 10010110b 
sub ah, 00001100b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 1001011000000000b
sub bx, 0101110011111111b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 0011011000000000b
sub bx, 1001110011111111b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 1001011000000000b 
sub cx, 1001110011111111b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 1001011000000000b 
sub cx, 1111001100000000b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

;;
;; Instruction Forms
;; 
; sub reg8,reg8 3   2   sub al,dl
_FACT_
mov ah, 1
mov al, 1
sub ah, al
ASSERT_ZERO_CMP {ah, 0}

; sub [mem8],reg8   16EA    2 to 4  sub [ByteVar],ah
_FACT_
mov ah, 1
sub byte [ByteVar], ah
ASSERT_ZERO_CMP {byte [ByteVar], 1}

; sub reg8,[mem8]   9EA 2 to 4  sub dl,[si1]
_FACT_
mov dl, 0
sub dl, byte [si1]
ASSERT_ZERO_CMP {dl, 0ffh}

; sub reg16,reg16   3   2   sub ax,dx
_FACT_
mov ax, 1
mov dx, 1
sub ax, dx
ASSERT_ZERO_CMP {ax, 0}

; sub [mem16],reg16 24EA    2 to 4  sub [WordVar],ax
_FACT_
mov ax, 1
sub word [WordVar], ax
ASSERT_ZERO_CMP {word [WordVar], 3}

; sub reg16,[mem16] 13EA    2 to 4  sub cx,[dibp]
_FACT_
mov cx, 0
sub cx, word [dibp]
ASSERT_ZERO_CMP {cx, 0fffdh}

; sub reg8,immed8   4   3   sub dl,10h
_FACT_
mov dl, 2
sub dl, 1
ASSERT_ZERO_CMP {dl, 1}

; sub [mem8],immed8 17EA    3 to 5  sub [ByteVar],01h
_FACT_
sub byte [ByteVar2], 1
ASSERT_ZERO_CMP {byte [ByteVar2], 4}

; sub reg16,sextimmed   4   3   sub dx,1
_FACT_
mov dx, 2
sub dx, -1
ASSERT_ZERO_CMP {dx, 3}

; sub reg16,immed16 4   4   sub dx,80h
_FACT_
mov dx, 0
sub dx, strict word -1
ASSERT_ZERO_CMP {dx, strict word 1}

; sub [mem16],sextimmed 25EA    3 to 5  sub word ptr [bp],10h
_FACT_
sub word [WordVar2], -1
ASSERT_ZERO_CMP {word [WordVar2], 10}

; sub [mem16],immed16   25EA    4 to 6  sub word ptr [bp],100h
_FACT_
sub word [WordVar3], strict word -2
ASSERT_ZERO_CMP {word [WordVar3], strict word 12}

; sub al,immed8 4   2   sub al,20h
; sub ax,immed16    4   3   sub ax,100h

__END_TEST_SUITE__
