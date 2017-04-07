%include "_TestFramework.inc"

BITS 16
ORG 100h

%macro UnsetFlags 0
    mov ax, 0
    push ax
    popf
%endmacro

%macro SetFlags 0
    mov ax, 0ffffh
    push ax
    popf
%endmacro

%macro GetFlags 0
    pushf
    pop ax
%endmacro

%define Mask_CF   0000000000000001b
%define Mask_PF   0000000000000100b
%define Mask_AF   0000000000010000b
%define Mask_ZF   0000000001000000b
%define Mask_SF   0000000010000000b
%define Mask_OF   0000100000000000b

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
test ax, Mask_CF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov bl, 1
add bl, 1
GetFlags
test ax, Mask_CF
ASSERT_ZERO

_FACT_
UnsetFlags
mov bx, 5
add bx, 0ffffh
GetFlags
test ax, Mask_CF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov cx, 1
add cx, 1
GetFlags
test ax, Mask_CF
ASSERT_ZERO

; PF
_FACT_
UnsetFlags
mov al, 82h
add al, 84h
GetFlags
test ax, Mask_PF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov dl, 83h
add dl, 84h
GetFlags
test ax, Mask_PF
ASSERT_ZERO

_FACT_
UnsetFlags
mov cx, 1
add cx, 0ffffh
GetFlags
test ax, Mask_PF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov bx, 1083h
add bx, 1084h
GetFlags
test ax, Mask_PF
ASSERT_ZERO

; AF
_FACT_
UnsetFlags
mov al, 0fh
add al, 1
GetFlags
test ax, Mask_AF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov dh, 0eh
add dh, 1
GetFlags
test ax, Mask_AF
ASSERT_ZERO

_FACT_
UnsetFlags
mov cx, 0eh
add cx, 2
GetFlags
test ax, Mask_AF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov bx, 1
add bx, 1
GetFlags
test ax, Mask_AF
ASSERT_ZERO

; ZF
_FACT_
UnsetFlags
mov dl, 1
add dl, 0ffh
GetFlags
test ax, Mask_ZF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov ch, 1
add ch, 1
GetFlags
test ax, Mask_ZF
ASSERT_ZERO

_FACT_
UnsetFlags
mov si, 1
add si, 0ffffh
GetFlags
test ax, Mask_ZF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov di, 1
add di, 1
GetFlags
test ax, Mask_ZF
ASSERT_ZERO

; SF
_FACT_
UnsetFlags
mov dl, 1
add dl, 0feh
GetFlags
test ax, Mask_SF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov ch, 1
add ch, 1
GetFlags
test ax, Mask_SF
ASSERT_ZERO

_FACT_
UnsetFlags
mov si, 1
add si, 0fffeh
GetFlags
test ax, Mask_SF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov di, 1
add di, 1
GetFlags
test ax, Mask_SF
ASSERT_ZERO

; OF
_FACT_
UnsetFlags
mov bl, 10010110b
add bl, 10100011b 
GetFlags
test ax, Mask_OF
ASSERT_NOT_ZERO

_FACT_
UnsetFlags
mov bh, 00110110b
add bh, 01100011b 
GetFlags
test ax, Mask_OF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov dl, 10010110b 
add dl, 01100011b 
GetFlags
test ax, Mask_OF
ASSERT_ZERO

_FACT_
SetFlags
mov ah, 10010110b 
add ah, 11110011b 
GetFlags
test ax, Mask_OF
ASSERT_ZERO

_FACT_
UnsetFlags
mov bx, 1001011000000000b
add bx, 1010001100000000b 
GetFlags
test ax, Mask_OF
ASSERT_NOT_ZERO

_FACT_
UnsetFlags
mov bx, 0011011000000000b
add bx, 0110001100000000b 
GetFlags
test ax, Mask_OF
ASSERT_NOT_ZERO

_FACT_
SetFlags
mov cx, 1001011000000000b 
add cx, 0110001100000000b 
GetFlags
test ax, Mask_OF
ASSERT_ZERO

_FACT_
SetFlags
mov cx, 1001011000000000b 
add cx, 1111001100000000b 
GetFlags
test ax, Mask_OF
ASSERT_ZERO

;;
;; Instruction Forms
;; 
; add reg8,reg8	3	2	add ah,al
_FACT_
mov ah, 1h
mov al, 2h
add ah, al
cmp ah, 3h
ASSERT_ZERO

; add [mem8],reg8	16EA	2 to 4	add [bx1],dh
_FACT_
mov dh, 2h
add [bx1], dh
cmp byte [bx1], 3h
ASSERT_ZERO

; add reg8,[mem8]	9EA	2 to 4	add ch,[bx]
_FACT_
mov ch, 1h
mov bx, bx2
add ch, [bx]
cmp ch, 3h
ASSERT_ZERO

; add reg16,reg16	3	2	add dx,ax
_FACT_
mov dx, 1h
mov ax, 2h
add dx, ax
cmp dx, 3h
ASSERT_ZERO

; add [mem16],reg16	24EA	2 to 4	add [bp5],ax
_FACT_
mov ax, 2h
add [bp5], ax
cmp word [bp5], 3h
ASSERT_ZERO

; add reg16,[mem16]	13EA	2 to 4	add ax,[Basedi]
_FACT_
mov ax, 1h
add ax, [Basedi]
cmp ax, 3h
ASSERT_ZERO

; add reg8,immed8	4	3	add dl,16
_FACT_
mov dl, 1h
add dl, 16
cmp dl, 17
ASSERT_ZERO

; add [mem8],immed8	17EA	3 to 5	add byte ptr [si6],0c3h
_FACT_
add byte [si6], 0c3h
cmp byte [si6], 0c4h
ASSERT_ZERO

; add reg16,sextimmed	4	3	add si,0ff80h
_FACT_
mov si, 1h
add si, 0ff80h
cmp si, 0ff81h
ASSERT_ZERO

; add reg16,immed16	4	4	add si,8000h
_FACT_
mov si, 1h
add si, 8000h
cmp si, 8001h
ASSERT_ZERO

; add [mem16],sextimmed	25EA	3 to 5	add [WordVar],3
_FACT_
add word [WordVar], 0ff80h
cmp word [WordVar], 0ff81h
ASSERT_ZERO

; add [mem16],immed16	25EA	4 to 6	add [WordVar],300h
_FACT_
add word [WordVar1], 8000h
cmp word [WordVar1], 8001h
ASSERT_ZERO

; add al,immed8	4	2	add al,1
_FACT_
mov al, 1h
add al, 1h
cmp al, 2h
ASSERT_ZERO

; add ax,immed16	4	3	add ax,2
_FACT_
mov ax, 1h
add ax, 2h
cmp ax, 3h
ASSERT_ZERO

__END_TEST_SUITE__
