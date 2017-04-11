%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Macros used in the tests
;; 
%macro LoopTestSimple 2
    mov byte [StartOfVars + 1], 0
    mov byte [StartOfVars + 2], 0
    mov byte [StartOfVars + 3], 0
    mov cx, 3
    SetFlags %2
    %%SumLoop:
    mov bx, cx
    mov byte [StartOfVars + bx], cl
  %ifidni %1,g
    loop %%SumLoop
  %else 
    loop%+1 %%SumLoop
  %endif   
    GetFlags
    and ax, Mask_AllF
    ASSERT_ZERO_CMP {ax, %2}
    ASSERT_ZERO_CMP {cx, 0}
    ASSERT_ZERO_CMP {byte [StartOfVars + 1], 1}
    ASSERT_ZERO_CMP {byte [StartOfVars + 2], 2}
    ASSERT_ZERO_CMP {byte [StartOfVars + 3], 3}
%endmacro

;;
;; Functions used in the tests
;;
; Function: strnlen
; In: 
; - SI: start of string
; - CX: max length
; Out:
; - BX: length of the string
strnlen:
    mov bx, 0
  strnlenloop:
    inc bx
    cmp byte [si+bx-1], 0 
    loopnz strnlenloop
    dec bx
    ret 

; Function: strncommon
; In:
; - DI: start of string 1 
; - SI: start of string 2
; - CX: max length
; Out:
; - BX: length of the common prefix
strncommon:
    mov bx, 0
  strncommonloop:
    inc bx
    mov dl, byte [di+bx-1] 
    cmp dl, byte [si+bx-1] 
    loopz strncommonloop
    dec bx
    ret 

;;
;; Variables used in the tests
;;
StartOfVars:
        db  0
        db  0
        db  0
        db  0
str0    db  0
str1    db  'hello',0 
str2    db  'hell',0 
str3    db  'xyz',0 

;;
;; Start of Tests
;;
_main:

; loop disp8	17 (CX<>0)/5 (CX=0)	2	loop WaitLoop
_FACT_
LoopTestSimple g, Mask_AllF

; loopnz disp8    19 (CX<>0 and ZF=0)/5 (CX=0 or ZF=1)    2    loopnz PollLp
_FACT_
LoopTestSimple nz, (Mask_AllF & ~Mask_ZF)

_FACT_
mov cx, 0x100
mov si, str0
call strnlen
ASSERT_ZERO_CMP {bx, 0}

_FACT_
mov cx, 0x100
mov si, str1
call strnlen
ASSERT_ZERO_CMP {bx, 5}

_FACT_
mov cx, 0x4
mov si, str1
call strnlen
ASSERT_ZERO_CMP {bx, 3}

; loopz disp8    18 (CX<>0 and ZF=1)/6 (CX=0 or ZF=0)    2    loopz MaxWtLp
_FACT_
LoopTestSimple z, Mask_AllF 

_FACT_
mov cx, 0x100
mov si, str2
mov di, str3
call strncommon
ASSERT_ZERO_CMP {bx, 0}

_FACT_
mov cx, 0x100
mov si, str1
mov di, str2
call strncommon
ASSERT_ZERO_CMP {bx, 4}

_FACT_
mov cx, 3
mov si, str1
mov di, str2
call strncommon
ASSERT_ZERO_CMP {bx, 2}

__END_TEST_SUITE__
