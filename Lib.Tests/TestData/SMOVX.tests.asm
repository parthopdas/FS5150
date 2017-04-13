%include "_TestFramework.inc"
%include "_Flags.inc"

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Functions used in the tests
;; 

;;
;; Variables used in the tests
;;
btst1src        db '1'
btst1dst        db 0
btst2src        db 'X','Y'
btst2dst        db 0,0
wtst1src        dw 'AA'
wtst1dst        dw 0
wtst2src        dw 'bb'
wtst2dst        dw 0
wtst3src        dw '44'
wtst3dst        dw '55'
wtst4src        dw 'MM','NN'
wtst4dst        dw 0,0
wtst5src        dw '88','99'
wtst5dst        dw 0,0
wtst6dst        dw 0,0

;;
;; Start of Tests
;;
_main:

;;
;; Form Tests
;;
;; [ ] MOVS/MOVSB/MOVSW
;; - DS:SI -> ES:DI
;;   - DS can be overridden
;; - rep with CX
;;   - After each repetition
;;     - Initial CX 0 => 0 repetitions
;;     - CX decremented after each iteration
;;     - SI/DI: if DF -- else ++
;; - No flags affected
; movsb 18  1   movsb
_FACT_ ; b: norep, di=1    => cx same, si/di deced, no flags, value copied
SetFlags
mov si, btst1src
mov di, btst1dst
mov cx, 100h
movsb
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {si, btst1src - 1}
ASSERT_EQUAL_CMP {di, btst1dst - 1}
mov bl, byte [btst1src]
ASSERT_EQUAL_CMP {bl, byte [btst1dst]}

; rep movsb 9(17*CX)    2   rep movsb
_FACT_ ; b: rep cx=2, di=0 => cx 0, si/di inced, no flags, value_s copied
UnsetFlags
mov si, btst2src
mov di, btst2dst
mov cx, 2
rep movsb
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, btst2src + 2}
ASSERT_EQUAL_CMP {di, btst2dst + 2}
mov bl, byte [btst2src]
ASSERT_EQUAL_CMP {bl, byte [btst2dst]}
mov bl, byte [btst2src + 1]
ASSERT_EQUAL_CMP {bl, byte [btst2dst + 1]}

; movsw 26  1   movsw
_FACT_ ; norep, di=0       => cx same, si/di inced, no flags, value copied
UnsetFlags
mov si, wtst1src
mov di, wtst1dst
mov cx, 100h
movsw
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {si, wtst1src + 2}
ASSERT_EQUAL_CMP {di, wtst1dst + 2}
mov bx, word [wtst1src]
ASSERT_EQUAL_CMP {bx, word [wtst1dst]}

_FACT_ ; norep, di=1       => cx same, si/di deced, no flags, value copied
SetFlags
mov si, wtst2src
mov di, wtst2dst
mov cx, 100h
movsw
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {si, wtst2src - 2}
ASSERT_EQUAL_CMP {di, wtst2dst - 2}
mov bx, word [wtst2src]
ASSERT_EQUAL_CMP {bx, word [wtst2dst]}

; rep movsw 9(25*CX)    2   rep movsw
_FACT_ ; rep cx=0          => cx same, si/di smme, no flags, value not copied
SetFlags
mov si, wtst3src
mov di, wtst3dst
mov cx, 0h
rep movsw
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, wtst3src}
ASSERT_EQUAL_CMP {di, wtst3dst}
ASSERT_EQUAL_CMP {word [wtst3src], '44'}
ASSERT_EQUAL_CMP {word [wtst3dst], '55'}

_FACT_ ; rep cx=2, di=0    => cx 0, si/di inced, no flags, value_s copied
UnsetFlags
mov si, wtst4src
mov di, wtst4dst
mov cx, 2
rep movsw
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, wtst4src + 4}
ASSERT_EQUAL_CMP {di, wtst4dst + 4}
mov bx, word [wtst4src]
ASSERT_EQUAL_CMP {bx, word [wtst4dst]}
mov bx, word [wtst4src + 2]
ASSERT_EQUAL_CMP {bx, word [wtst4dst + 2]}

_FACT_ ; rep cx=2, di=1    => cx 0, si/di deced, no flags, value_s copied
SetFlags
mov si, wtst5src + 2
mov di, wtst5dst + 2
mov cx, 2
rep movsw
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, wtst5src - 2}
ASSERT_EQUAL_CMP {di, wtst5dst - 2}
mov bx, word [wtst5src]
ASSERT_EQUAL_CMP {bx, word [wtst5dst]}
mov bx, word [wtst5src + 2]
ASSERT_EQUAL_CMP {bx, word [wtst5dst + 2]}

_FACT_ ; ss: rep cx=1, di=1=> cx 0, si/di deced, no flags, value_s copied
std
mov ax, '--'
push ax
mov ax, 0ffh
mov ds, ax
mov si, sp
mov di, wtst6dst
mov cx, 1
rep ss movsw
pop ax
mov ax, cs
mov ds, ax
mov bx, word [wtst6dst]
ASSERT_EQUAL_CMP {bx, '--'}

__END_TEST_SUITE__
