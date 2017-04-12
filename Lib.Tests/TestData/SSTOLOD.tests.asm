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
ltest1str    db  'A'
ltest2str    db  '1', '2'
ltest1wstr   dw  'AA'
ltest2wstr   dw  '11', '22'
stest1str    db  0
stest2str    db  0, 0
stest1wstr   dw  0
stest2wstr   dw  0, 0

;;
;; Start of Tests
;;
_main:
    
;;
;; Form Tests
;;
;; [v] LODS/LODSB/LODSW 
;; - mov A? ES:DI
;; - rep with CX
;;   - After each repetition
;;     - Initial CX 0 => 0 repetitions
;;     - CX decremented after each iteration
;;     - SI/DI: if DF -- else ++
;; - No flags affected
; lodsb 12  1   lodsb
_FACT_ ; norep, df=0 => load, no update of CX, update DI, no flags affected
UnsetFlags
mov cx, 100h
mov ax, cs
mov ds, ax
mov si, ltest1str
lodsb
mov bx, ax
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {bl, 'A'}
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {si, ltest1str + 1}

_FACT_ ; norep, df=1 => load, no update of CX, update DI, no flags affected
SetFlags
mov cx, 100h
mov ax, cs
mov ds, ax
mov si, ltest1str
lodsb
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bl, 'A'}
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {si, ltest1str - 1}

; rep lodsb 9(13*CX)    2   rep lodsb
_FACT_ ; rep cx=2, df=0 => load, update of CX, update DI, no flags affected
UnsetFlags
mov cx, 2
mov ax, cs
mov ds, ax
mov si, ltest2str
rep lodsb
mov bx, ax
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {bl, '2'}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, ltest2str + 2}

_FACT_ ; rep cx=2, df=1 => load, update of CX, update DI, no flags affected
SetFlags
mov cx, 2
mov ax, cs
mov ds, ax
mov si, ltest2str + 1
rep lodsb
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bl, '1'}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, ltest2str - 1}

_FACT_ ; rep cx=0 => no load, no update of CX, no update DI, no flags affected
SetFlags
mov cx, 0
mov ax, cs
mov ds, ax
mov al, 0feh
mov si, ltest2str
rep lodsb
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bl, 0feh}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, ltest2str}

; lodsw 16  1   lodsw
_FACT_ ; norep, df=0 => load, no update of CX, update DI, no flags affected
UnsetFlags
mov cx, 100h
mov ax, cs
mov ds, ax
mov si, ltest1wstr
lodsw
mov bx, ax
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {bx, 'AA'}
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {si, ltest1wstr + 2}

; rep lodsw 9(17*CX)    2   rep lodsw
_FACT_ ; rep cx=2, df=1 => load, update of CX, update DI, no flags affected
SetFlags
mov cx, 2
mov ax, cs
mov ds, ax
mov si, ltest2wstr + 2
rep lodsw
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bx, '11'}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {si, ltest2wstr - 2}

;;
;; Form Tests
;;
;; [v] STOS/STOSB/STOSW
;; - mov ES:DI A?
;; - rep with CX
;;   - After each repetition
;;     - Initial CX 0 => 0 repetitions
;;     - CX decremented after each iteration
;;     - SI/DI: if DF -- else ++
;; - No flags affected

; stosb 11  1   stosb
_FACT_ ; norep, df=0 => load, no update of CX, update DI, no flags affected
UnsetFlags
mov cx, 100h
mov ax, cs
mov es, ax
mov di, stest1str
mov al, 'x'
stosb
mov bx, ax
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {bl, 'x'}
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {di, stest1str + 1}
ASSERT_EQUAL_CMP {byte [stest1str], 'x'}

_FACT_ ; norep, df=1 => load, no update of CX, update DI, no flags affected
SetFlags
mov cx, 100h
mov ax, cs
mov es, ax
mov di, stest1str
mov al, 'y'
stosb
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bl, 'y'}
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {di, stest1str - 1}
ASSERT_EQUAL_CMP {byte [stest1str], 'y'}

; rep stosb 9(10*CX)    2   rep stosb
_FACT_ ; rep cx=2, df=0 => load, update of CX, update DI, no flags affected
UnsetFlags
mov cx, 2
mov ax, cs
mov es, ax
mov di, stest2str
mov al, '5'
rep stosb
mov bx, ax
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {bl, '5'}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {di, stest2str + 2}
ASSERT_EQUAL_CMP {byte [stest2str], '5'}
ASSERT_EQUAL_CMP {byte [stest2str + 1], '5'}

_FACT_ ; rep cx=2, df=1 => load, update of CX, update DI, no flags affected
SetFlags
mov cx, 2
mov ax, cs
mov es, ax
mov di, stest2str + 1
mov al, '7'
rep stosb
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bl, '7'}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {di, stest2str - 1}
ASSERT_EQUAL_CMP {byte [stest2str + 1], '7'}
ASSERT_EQUAL_CMP {byte [stest2str], '7'}

_FACT_ ; rep cx=0 => no load, no update of CX, no update DI, no flags affected
SetFlags
mov cx, 0
mov ax, cs
mov es, ax
mov al, 0feh
mov di, ltest1str
rep stosb
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bl, 0feh}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {di, ltest1str}
ASSERT_EQUAL_CMP {byte [ltest1str], 'A'}

; stosw 15  1   stosw
_FACT_ ; norep, df=0 => load, no update of CX, update DI, no flags affected
UnsetFlags
mov cx, 100h
mov ax, cs
mov es, ax
mov di, stest1wstr
mov ax, 'MM'
stosw
mov bx, ax
GetFlagsAndVerify Mask_NoF
ASSERT_EQUAL_CMP {bx, 'MM'}
ASSERT_EQUAL_CMP {cx, 100h}
ASSERT_EQUAL_CMP {di, stest1wstr + 2}
ASSERT_EQUAL_CMP {word [stest1wstr], 'MM'}

; rep stosw 9(14*CX)    2   rep stosw
_FACT_ ; rep cx=2, df=1 => load, update of CX, update DI, no flags affected
SetFlags
mov cx, 2
mov ax, cs
mov es, ax
mov di, stest2wstr + 2
mov ax, 'NN'
rep stosw
mov bx, ax
GetFlagsAndVerify Mask_AllF
ASSERT_EQUAL_CMP {bx, 'NN'}
ASSERT_EQUAL_CMP {cx, 0}
ASSERT_EQUAL_CMP {di, stest2wstr - 2}
ASSERT_EQUAL_CMP {word [stest2wstr + 2], 'NN'}
ASSERT_EQUAL_CMP {word [stest2wstr], 'NN'}


__END_TEST_SUITE__
