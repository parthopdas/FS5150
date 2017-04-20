%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables use in the tests
;;
bx1         db      1h
bx2         db      2h
bp5         dw      1h
Basedi      dw      2h
si6         db      1h
WordVar     dw      1h
WordVar1    dw      1h

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
add ah, 0ffh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov bl, 1
add bl, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

_FACT_
UnsetFlags
mov bx, 5
add bx, 0ffffh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_CF}

_FACT_
SetFlags
mov cx, 1
add cx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_CF}

; PF
_FACT_
UnsetFlags
mov al, 82h
add al, 84h
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov dl, 83h
add dl, 84h
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

_FACT_
UnsetFlags
mov cx, 1
add cx, 0ffffh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov bx, 1083h
add bx, 1084h
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

; AF
_FACT_
UnsetFlags
mov al, 0fh
add al, 1
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_AF}

_FACT_
SetFlags
mov dh, 0eh
add dh, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_AF}

_FACT_
UnsetFlags
mov cx, 0eh
add cx, 2
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_AF}

_FACT_
SetFlags
mov bx, 1
add bx, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_AF}

; ZF
_FACT_
UnsetFlags
mov dl, 1
add dl, 0ffh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov ch, 1
add ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

_FACT_
UnsetFlags
mov si, 1
add si, 0ffffh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov di, 1
add di, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

; SF
_FACT_
UnsetFlags
mov dl, 1
add dl, 0feh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov ch, 1
add ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

_FACT_
UnsetFlags
mov si, 1
add si, 0fffeh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov di, 1
add di, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

; OF
_FACT_
UnsetFlags
mov bl, 10010110b
add bl, 10100011b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bh, 00110110b
add bh, 01100011b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov dl, 10010110b 
add dl, 01100011b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov ah, 10010110b 
add ah, 11110011b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 1001011000000000b
add bx, 1010001100000000b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
UnsetFlags
mov bx, 0011011000000000b
add bx, 0110001100000000b 
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 1001011000000000b 
add cx, 0110001100000000b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

_FACT_
SetFlags
mov cx, 1001011000000000b 
add cx, 1111001100000000b 
GetFlags
ASSERT_ZERO_TEST {ax, Mask_OF}

;;
;; Instruction Forms
;; 
; add reg8,reg8	3	2	add ah,al
_FACT_
mov ah, 1h
mov al, 2h
add ah, al
ASSERT_ZERO_CMP {ah, 3h}

; add [mem8],reg8	16EA	2 to 4	add [bx1],dh
_FACT_
mov dh, 2h
add [bx1], dh
ASSERT_ZERO_CMP {byte [bx1], 3h}

; add reg8,[mem8]	9EA	2 to 4	add ch,[bx]
_FACT_
mov ch, 1h
mov bx, bx2
add ch, [bx]
ASSERT_ZERO_CMP {ch, 3h}

; add reg16,reg16	3	2	add dx,ax
_FACT_
mov dx, 1h
mov ax, 2h
add dx, ax
ASSERT_ZERO_CMP {dx, 3h}

; add [mem16],reg16	24EA	2 to 4	add [bp5],ax
_FACT_
mov ax, 2h
add [bp5], ax
ASSERT_ZERO_CMP {word [bp5], 3h}

; add reg16,[mem16]	13EA	2 to 4	add ax,[Basedi]
_FACT_
mov ax, 1h
add ax, [Basedi]
ASSERT_ZERO_CMP {ax, 3h}

; add reg8,immed8	4	3	add dl,16
_FACT_
mov dl, 1h
add dl, 16
ASSERT_ZERO_CMP {dl, 17}

; add [mem8],immed8	17EA	3 to 5	add byte ptr [si6],0c3h
_FACT_
add byte [si6], 0c3h
ASSERT_ZERO_CMP {byte [si6], 0c4h}

; add reg16,sextimmed	4	3	add si,0ff80h
_FACT_
mov si, 1h
add si, 0ff80h
ASSERT_ZERO_CMP {si, 0ff81h}

; add reg16,immed16	4	4	add si,8000h
_FACT_
mov si, 1h
add si, strict word 8000h
ASSERT_ZERO_CMP {si, strict word 8001h}

; add [mem16],sextimmed	25EA	3 to 5	add [WordVar],3
_FACT_
add word [WordVar], -1
ASSERT_ZERO_CMP {word [WordVar], 0}

; add [mem16],immed16	25EA	4 to 6	add [WordVar],300h
_FACT_
add word [WordVar1], strict word 8000h
ASSERT_ZERO_CMP {word [WordVar1], strict word 8001h}

; add al,immed8	4	2	add al,1
_FACT_
mov al, 1h
add al, 1h
ASSERT_ZERO_CMP {al, 2h}

; add ax,immed16	4	3	add ax,2
_FACT_
mov ax, 1h
add ax, strict word 2h
ASSERT_ZERO_CMP {ax, strict word 3h}

__END_TEST_SUITE__
