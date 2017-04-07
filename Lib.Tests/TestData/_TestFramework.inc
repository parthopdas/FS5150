%macro _FACT_ 0 
    inc word [NumberOfTests]
%endmacro

%macro ASSERT_ZERO 0
    jz $+5
    jmp near _ATestFailed
%endmacro

%macro ASSERT_NOT_ZERO 0
    jnz $+5
    jmp near _ATestFailed
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
