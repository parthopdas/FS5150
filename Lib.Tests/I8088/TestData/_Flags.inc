%macro SetFlags 1
    mov ax, %1
    push ax
    popf
%endmacro

%macro UnsetFlags 0
    SetFlags 0
%endmacro

%macro SetFlags 0
    SetFlags 0ffffh
%endmacro

%macro GetFlags 0
    pushf
    pop ax
%endmacro

%macro GetFlagsAndVerify 1
    GetFlags
    and ax, Mask_AllF
    ASSERT_ZERO_CMP {ax, %1}
%endmacro

%define Mask_NoF  0000000000000000b
%define Mask_CF   0000000000000001b
%define Mask_PF   0000000000000100b
%define Mask_AF   0000000000010000b
%define Mask_ZF   0000000001000000b
%define Mask_SF   0000000010000000b
%define Mask_DF   0000010000000000b
%define Mask_OF   0000100000000000b
%define Mask_AllF 0000110011010101b
