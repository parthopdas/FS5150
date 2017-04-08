%macro _FACT_ 0 
    inc word [NumberOfTests]
%endmacro

%macro PVT_ASSERT_ZERO 0
    jz $+5
    jmp near _ATestFailed
%endmacro

%macro PVT_ASSERT_NOT_ZERO 0
    jnz $+5
    jmp near _ATestFailed
%endmacro

%macro ASSERT_ZERO_TEST 1
    test %1
    PVT_ASSERT_ZERO
%endmacro

%macro ASSERT_ZERO_CMP 1
    cmp %1
    PVT_ASSERT_ZERO
%endmacro

%macro ASSERT_NOT_ZERO_TEST 1
    test %1
    PVT_ASSERT_NOT_ZERO
%endmacro

%macro __BEGIN_TEST_SUITE__ 0
mov ax, cs
mov ss, ax
mov sp, StackEnd
jmp near _main

StackStart:
    times 32    dw  0
StackEnd:
NumberOfTests   dw  0

_ATestFailed:
hlt

_AllTestsPassed:
hlt
%endmacro

%macro __END_TEST_SUITE__ 0
jmp near _AllTestsPassed
%endmacro
