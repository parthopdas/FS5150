; ADD Tests TODO
; v 8 bit and 16 bit
; v Variations from MA ZofALP
; - make DRY with macros
; - Flags tests

BITS 16
ORG 100h

%macro TestBegin 0 
    inc word [NumberOfTests]
%endmacro

%macro TestEnd 0
    jz $+5
    jmp near _OneTestFailed
%endmacro

jmp near _main

NumberOfTests   dw  0

_OneTestFailed:
hlt

_AllTestPassed:
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

;;
;; Start of Tests
;;
_main:

;;
;; Instruction Forms
;; 
; add reg8,reg8	3	2	add ah,al
TestBegin
mov ah, 1h
mov al, 2h
add ah, al
cmp ah, 3h
TestEnd

; add [mem8],reg8	16EA	2 to 4	add [bx1],dh
TestBegin
mov dh, 2h
add [bx1], dh
cmp byte [bx1], 3h
TestEnd

; add reg8,[mem8]	9EA	2 to 4	add ch,[bx]
TestBegin
mov ch, 1h
mov bx, bx2
add ch, [bx]
cmp ch, 3h
TestEnd

; add reg16,reg16	3	2	add dx,ax
TestBegin
mov dx, 1h
mov ax, 2h
add dx, ax
cmp dx, 3h
TestEnd

; add [mem16],reg16	24EA	2 to 4	add [bp5],ax
TestBegin
mov ax, 2h
add [bp5], ax
cmp word [bp5], 3h
TestEnd

; add reg16,[mem16]	13EA	2 to 4	add ax,[Basedi]
TestBegin
mov ax, 1h
add ax, [Basedi]
cmp ax, 3h
TestEnd

; add reg8,immed8	4	3	add dl,16
TestBegin
mov dl, 1h
add dl, 16
cmp dl, 17
TestEnd

; add [mem8],immed8	17EA	3 to 5	add byte ptr [si6],0c3h
TestBegin
add byte [si6], 0c3h
cmp byte [si6], 0c4h
TestEnd

; add reg16,sextimmed	4	3	add si,0ff80h
TestBegin
mov si, 1h
add si, 0ff80h
cmp si, 0ff81h
TestEnd

; add reg16,immed16	4	4	add si,8000h
TestBegin
mov si, 1h
add si, 8000h
cmp si, 8001h
TestEnd

; add [mem16],sextimmed	25EA	3 to 5	add [WordVar],3
TestBegin
add word [WordVar], 0ff80h
cmp word [WordVar], 0ff81h
TestEnd

; add [mem16],immed16	25EA	4 to 6	add [WordVar],300h
TestBegin
add word [WordVar1], 8000h
cmp word [WordVar1], 8001h
TestEnd

; add al,immed8	4	2	add al,1
TestBegin
mov al, 1h
add al, 1h
cmp al, 2h
TestEnd

; add ax,immed16	4	3	add ax,2
TestBegin
mov ax, 1h
add ax, 2h
cmp ax, 3h
TestEnd

;;
;; End of Tests
;;
jmp near _AllTestPassed
