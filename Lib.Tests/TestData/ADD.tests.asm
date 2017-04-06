BITS 16
ORG 100h

%macro __FACT__ 0 
    inc word [NumberOfTests]
%endmacro

%macro AssertZero 0
    jz $+5
    jmp near _ATestFailed
%endmacro

%macro AssertNotZero 0
    jnz $+5
    jmp near _ATestFailed
%endmacro

mov ax, cs
mov ss, ax
mov sp, StackBase
jmp near _main

NumberOfTests   dw  0

_ATestFailed:
hlt

_AllTestsPassed:
hlt

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
; TODO: There is a bug somewhere here. 
; Running all the tests overflows the stack limit. It shouldn't 
; as all we are doing is balanced sets of PUSH/POP.
Stack       dw      0f0f0h
            dw      0f0f0h
            dw      0f0f0h
            dw      0f0f0h
            dw      0f0f0h
            dw      0f0f0h
            dw      0f0f0h
            dw      0f0f0h
StackBase:

;;
;; Start of Tests
;;
_main:

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

;;
;; Flags Tests
;; 
; CF
__FACT__
UnsetFlags
mov ah, 1
add ah, 0ffh
GetFlags
test ax, Mask_CF
AssertNotZero

__FACT__
SetFlags
mov bl, 1
add bl, 1
GetFlags
test ax, Mask_CF
AssertZero

__FACT__
UnsetFlags
mov bx, 5
add bx, 0ffffh
GetFlags
test ax, Mask_CF
AssertNotZero

__FACT__
SetFlags
mov cx, 1
add cx, 1
GetFlags
test ax, Mask_CF
AssertZero

; PF
__FACT__
UnsetFlags
mov al, 82h
add al, 84h
GetFlags
test ax, Mask_PF
AssertNotZero

__FACT__
SetFlags
mov dl, 83h
add dl, 84h
GetFlags
test ax, Mask_PF
AssertZero

__FACT__
UnsetFlags
mov cx, 1
add cx, 0ffffh
GetFlags
test ax, Mask_PF
AssertNotZero

__FACT__
SetFlags
mov bx, 1083h
add bx, 1084h
GetFlags
test ax, Mask_PF
AssertZero

; AF
__FACT__
UnsetFlags
mov al, 0fh
add al, 1
GetFlags
test ax, Mask_AF
AssertNotZero

__FACT__
SetFlags
mov dh, 0eh
add dh, 1
GetFlags
test ax, Mask_AF
AssertZero

__FACT__
UnsetFlags
mov cx, 0eh
add cx, 2
GetFlags
test ax, Mask_AF
AssertNotZero

__FACT__
SetFlags
mov bx, 1
add bx, 1
GetFlags
test ax, Mask_AF
AssertZero

; ZF
__FACT__
UnsetFlags
mov dl, 1
add dl, 0ffh
GetFlags
test ax, Mask_ZF
AssertNotZero

__FACT__
SetFlags
mov ch, 1
add ch, 1
GetFlags
test ax, Mask_ZF
AssertZero

__FACT__
UnsetFlags
mov si, 1
add si, 0ffffh
GetFlags
test ax, Mask_ZF
AssertNotZero

__FACT__
SetFlags
mov di, 1
add di, 1
GetFlags
test ax, Mask_ZF
AssertZero

; SF
__FACT__
UnsetFlags
mov dl, 1
add dl, 0feh
GetFlags
test ax, Mask_SF
AssertNotZero

__FACT__
SetFlags
mov ch, 1
add ch, 1
GetFlags
test ax, Mask_SF
AssertZero

__FACT__
UnsetFlags
mov si, 1
add si, 0fffeh
GetFlags
test ax, Mask_SF
AssertNotZero

__FACT__
SetFlags
mov di, 1
add di, 1
GetFlags
test ax, Mask_SF
AssertZero

; OF
__FACT__
UnsetFlags
mov bl, 10010110b
add bl, 10100011b 
GetFlags
test ax, Mask_OF
AssertNotZero

__FACT__
UnsetFlags
mov bh, 00110110b
add bh, 01100011b 
GetFlags
test ax, Mask_OF
AssertNotZero

__FACT__
SetFlags
mov dl, 10010110b 
add dl, 01100011b 
GetFlags
test ax, Mask_OF
AssertZero

__FACT__
SetFlags
mov ah, 10010110b 
add ah, 11110011b 
GetFlags
test ax, Mask_OF
AssertZero

__FACT__
UnsetFlags
mov bx, 1001011000000000b
add bx, 1010001100000000b 
GetFlags
test ax, Mask_OF
AssertNotZero

__FACT__
UnsetFlags
mov bx, 0011011000000000b
add bx, 0110001100000000b 
GetFlags
test ax, Mask_OF
AssertNotZero

__FACT__
SetFlags
mov cx, 1001011000000000b 
add cx, 0110001100000000b 
GetFlags
test ax, Mask_OF
AssertZero

__FACT__
SetFlags
mov cx, 1001011000000000b 
add cx, 1111001100000000b 
GetFlags
test ax, Mask_OF
AssertZero

;;
;; Instruction Forms
;; 
; add reg8,reg8	3	2	add ah,al
__FACT__
mov ah, 1h
mov al, 2h
add ah, al
cmp ah, 3h
AssertZero

; add [mem8],reg8	16EA	2 to 4	add [bx1],dh
__FACT__
mov dh, 2h
add [bx1], dh
cmp byte [bx1], 3h
AssertZero

; add reg8,[mem8]	9EA	2 to 4	add ch,[bx]
__FACT__
mov ch, 1h
mov bx, bx2
add ch, [bx]
cmp ch, 3h
AssertZero

; add reg16,reg16	3	2	add dx,ax
__FACT__
mov dx, 1h
mov ax, 2h
add dx, ax
cmp dx, 3h
AssertZero

; add [mem16],reg16	24EA	2 to 4	add [bp5],ax
__FACT__
mov ax, 2h
add [bp5], ax
cmp word [bp5], 3h
AssertZero

; add reg16,[mem16]	13EA	2 to 4	add ax,[Basedi]
__FACT__
mov ax, 1h
add ax, [Basedi]
cmp ax, 3h
AssertZero

; add reg8,immed8	4	3	add dl,16
__FACT__
mov dl, 1h
add dl, 16
cmp dl, 17
AssertZero

; add [mem8],immed8	17EA	3 to 5	add byte ptr [si6],0c3h
__FACT__
add byte [si6], 0c3h
cmp byte [si6], 0c4h
AssertZero

; add reg16,sextimmed	4	3	add si,0ff80h
__FACT__
mov si, 1h
add si, 0ff80h
cmp si, 0ff81h
AssertZero

; add reg16,immed16	4	4	add si,8000h
__FACT__
mov si, 1h
add si, 8000h
cmp si, 8001h
AssertZero

; add [mem16],sextimmed	25EA	3 to 5	add [WordVar],3
__FACT__
add word [WordVar], 0ff80h
cmp word [WordVar], 0ff81h
AssertZero

; add [mem16],immed16	25EA	4 to 6	add [WordVar],300h
__FACT__
add word [WordVar1], 8000h
cmp word [WordVar1], 8001h
AssertZero

; add al,immed8	4	2	add al,1
__FACT__
mov al, 1h
add al, 1h
cmp al, 2h
AssertZero

; add ax,immed16	4	3	add ax,2
__FACT__
mov ax, 1h
add ax, 2h
cmp ax, 3h
AssertZero

;;
;; End of Tests
;;
jmp near _AllTestsPassed
