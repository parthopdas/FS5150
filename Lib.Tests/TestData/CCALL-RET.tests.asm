;; TODO: 
;; - Support parsing this instruction and then add the tests for it
;;   call [mem32]	53EA	2 to 4	call dword ptr [FarVec]

%include "_TestFramework.inc"

BITS 16
ORG 100h

%macro SaveStackRegs 0 
push ss
push sp
%endmacro

%macro VerifySavedStackRegs 0 
pop ax
mov bx, sp
ASSERT_ZERO_CMP {ax, bx}
pop ax
mov bx, ss
ASSERT_ZERO_CMP {ax, bx}
%endmacro

__BEGIN_TEST_SUITE__

;;
;; Functions used in the tests
;; 
CallRetNear:
    mov ax, 0deadh
    retn 

CallRetFar:
    mov ax, 0f00dh
    retf

CallRetNNear:
    mov ax, 0beefh
    retn 2

CallRetNFar:
    mov ax, 0baadh
    retf 4
;;
;; Variables used in the tests
;;
Vecssi      dw  0

;;
;; Start of Tests
;;
_main:

; call disp16	23	3	call near ptr NearTarget/retn	20	1	ret (in near proc) 
_FACT_
SaveStackRegs
mov ax, 0
call near CallRetNear
ASSERT_ZERO_CMP {ax, 0deadh}
VerifySavedStackRegs

; call disp16	23	3	call near ptr NearTarget/retn immed16	24	3	retn 10
_FACT_
SaveStackRegs
mov ax, 0
push ax
call near CallRetNNear
ASSERT_ZERO_CMP {ax, 0beefh}
VerifySavedStackRegs

; call reg16	20	2	call bx/retn	20	1	ret (in near proc)
_FACT_
SaveStackRegs
mov ax, 0
mov bx, CallRetNear
call bx
ASSERT_ZERO_CMP {ax, 0deadh}
VerifySavedStackRegs

; call [mem16]	29EA	2 to 4	call word ptr [Vecssi]/retn	20	1	ret (in near proc)
_FACT_
SaveStackRegs
mov ax, 0
mov word [Vecssi], CallRetNear
call word [Vecssi]
ASSERT_ZERO_CMP {ax, 0deadh}
VerifySavedStackRegs

; call segment:offset	36	5	call far ptr FarTarget/retf	34	1	retf
; NOTE: Cannot use this format in com files, hence the self-modifying code
_FACT_
SaveStackRegs
mov ax, 0
mov word [CallRetFarCodeCS], cs
mov word [CallRetFarCodeIP], CallRetFar
CallRetFarCode      db  9Ah
CallRetFarCodeIP    dw  0h
CallRetFarCodeCS    dw  0h
ASSERT_ZERO_CMP {ax, 0f00dh}
VerifySavedStackRegs

; call segment:offset	36	5	call far ptr FarTarget/retf immed16	33	3	ret 512 (in far proc)
; NOTE: Cannot use this format in com files, hence the self-modifying code
_FACT_
SaveStackRegs
mov ax, 0
push ax
push ax
mov word [CallRetNFarCodeCS], cs
mov word [CallRetNFarCodeIP], CallRetNFar
CallRetNFarCode      db  9Ah
CallRetNFarCodeIP    dw  0h
CallRetNFarCodeCS    dw  0h
ASSERT_ZERO_CMP {ax, 0baadh}
VerifySavedStackRegs

__END_TEST_SUITE__
