%macro _FACT_ 0 
    inc word [NumberOfTests]
%endmacro

%macro PVT_ASSERT_ZERO 0
    jz $+5
    jmp near TF_ATestFailed
%endmacro

%macro PVT_ASSERT_NOT_ZERO 0
    jnz $+5
    jmp near TF_ATestFailed
%endmacro

%macro ASSERT_ZERO_TEST 1
    test %1
    PVT_ASSERT_ZERO
%endmacro

%macro ASSERT_ZERO_CMP 1
    cmp %1
    PVT_ASSERT_ZERO
%endmacro

%macro ASSERT_EQUAL_CMP 1
    cmp %1
    PVT_ASSERT_ZERO
%endmacro

%macro ASSERT_NOT_ZERO_TEST 1
    test %1
    PVT_ASSERT_NOT_ZERO
%endmacro

%macro __BEGIN_TEST_SUITE__ 0
mov ax, cs
mov ds, ax
mov es, ax
mov ss, ax
mov sp, StackEnd
jmp near _main

StackStart:
    times 32    dw  0
StackEnd:
NumberOfTests   dw  0

TF_ATestFailed:
mov dx, TF_FailedMsg
call TF_PrintMessage
call TF_Quit
hlt

TF_AllTestsPassed:
mov dx, TF_PassedMsg
call TF_PrintMessage
call TF_Quit
hlt
%endmacro

%macro __END_TEST_SUITE__ 0
jmp near TF_AllTestsPassed

TF_PrintMessage:
    mov ah, 9
    int 21h
    ret

TF_Quit:
    mov al, 0
    mov ah, 04ch
    int 21h
    ret

TF_PassedMsg    db  'All tests passed!', 0ah, 0dh, '$'
TF_FailedMsg    db  'Test failed!', 0ah, 0dh, '$'
%endmacro
