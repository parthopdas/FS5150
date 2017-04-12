%include "_TestFramework.inc"
%include "_Flags.inc"

; TODO
; - Rename COM tests file according to instruction classes
; - Print tests succeeded or failed

BITS 16
ORG 100h

__BEGIN_TEST_SUITE__

;;
;; Variables use in the tests
;;
ByteVar     db  0f0h
WordVar     dw  0f00fh

;;
;; Start of Tests
;;
_main:

;;
;; Instruction Forms
;; 
; not reg8  3   2   not al
_FACT_
SetFlags
mov bl, 01010101b
not bl
GetFlagsAndVerify Mask_AllF
ASSERT_ZERO_CMP {bl, 10101010b}

; not [mem8]    16EA    2 to 4  not byte ptr [bx]
_FACT_
SetFlags
mov bx, ByteVar
not byte [bx]
GetFlagsAndVerify Mask_AllF
ASSERT_ZERO_CMP {byte [bx], 0fh}

; not reg16 3   2   not dx
_FACT_
SetFlags
mov dx, 0101010110101010b
not dx
GetFlagsAndVerify Mask_AllF
ASSERT_ZERO_CMP {dx, 1010101001010101b}

; not [mem16]   24EA    2 to 4  not [WordVar]
_FACT_
SetFlags
not word [WordVar]
GetFlagsAndVerify Mask_AllF
ASSERT_ZERO_CMP {word [WordVar], 0ff0h}

__END_TEST_SUITE__
