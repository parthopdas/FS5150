%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables use in the tests
;;
ByteVar     db  0
ByteVar1    db  0ffh
ByteVar2    db  1
bpsi        dw  0ff00h
WordVar     dw  01111h

;;
;; Start of Tests
;;
_main:

;;
;; Flags Tests
;; 
; PF
_FACT_
UnsetFlags
mov al, 2
or al, 4
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov dl, 3
or dl, 4
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

_FACT_
UnsetFlags
mov cx, 0
or cx, 0
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_PF}

_FACT_
SetFlags
mov bx, 1001h
or bx, 1000h
GetFlags
ASSERT_ZERO_TEST {ax, Mask_PF}

; ZF
_FACT_
UnsetFlags
mov dl, 0
or dl, 0
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov ch, 1
or ch, 0
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

_FACT_
UnsetFlags
mov si, 0
or si, 0
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_ZF}

_FACT_
SetFlags
mov di, 0
or di, 0xffff
GetFlags
ASSERT_ZERO_TEST {ax, Mask_ZF}

; SF
_FACT_
UnsetFlags
mov dl, 1
or dl, 0feh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}
    
_FACT_
SetFlags
mov ch, 1
or ch, 1
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

_FACT_
UnsetFlags
mov si, 1
or si, 0fffeh
GetFlags
ASSERT_NOT_ZERO_TEST {ax, Mask_SF}

_FACT_
SetFlags
mov di, 1
or di, 7000h
GetFlags
ASSERT_ZERO_TEST {ax, Mask_SF}

;;
;; Instruction Forms
;; 
; or reg8,reg8 3   2   or al,dl
_FACT_
SetFlags
mov dh, 0
mov dl, 1
or dh, dl
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {dh, 1}

; or [mem8],reg8   16EA    2 to 4  or [ByteVar],ch
_FACT_
SetFlags
mov dl, 0
or byte [ByteVar], dl
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {byte [ByteVar], 0}

; or reg8,[mem8]   9EA 2 to 4  or bh,[si]
_FACT_
SetFlags
mov si, ByteVar2
mov bh, 1
or bh, byte [si]
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {bh, 1}

; or reg16,reg16   3   2   or bp,ax
_FACT_
SetFlags
mov bp, 055aah
mov bx, 0aa55h
or bp, bx
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {bp, 0ffffh}

; or [mem16],reg16 24EA    2 to 4  or [bpsi],cx
_FACT_
SetFlags
mov cx, 0eh
or word [bpsi], cx
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {word [bpsi], 0ff0eh}

; or reg16,[mem16] 13EA    2 to 4  or ax,[bx]
_FACT_
SetFlags
mov bx, WordVar
mov ax, 2222h
or word [bx], ax
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {word [bx], 03333h}

; or reg8,immed8   4   3   or cl,03h
_FACT_
SetFlags
mov cl, 77h
or cl, 88h
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {cl, 0ffh}

; or [mem8],immed8 17EA    3 to 5  or [ByteVar1],29h
_FACT_
SetFlags
or byte [ByteVar], 11h
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {byte [ByteVar1], 0ffh}

; or reg16,sextimmed   4   3   or ax,01fh
_FACT_
SetFlags
mov cx, 0fh
or cx, -16
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {cx, 0ffffh}

; or reg16,immed16 4   4   or ax,01fffh
_FACT_
SetFlags
mov cx, 0fffh
or cx, 0e000h
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {cx, 0efffh}

; or [mem16],sextimmed 25EA    3 to 5  or [WordVar],7fh
_FACT_
SetFlags
mov word [WordVar], 055aah
or word [WordVar], -1
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {word [WordVar], 0ffffh}

; or [mem16],immed16   25EA    4 to 6  or [WordVar],7fffh
_FACT_
SetFlags
mov word [WordVar], 0fffh
or word [WordVar], 0e000h
GetFlags
and ax, (Mask_CF | Mask_OF)
ASSERT_ZERO_CMP {ax, 0}
ASSERT_ZERO_CMP {word [WordVar], 0efffh}

; or al,immed8 4   2   or al,0c0h
; - Covered above
; or ax,immed16    4   3   or ax,01ffh
; - Covered above

__END_TEST_SUITE__
