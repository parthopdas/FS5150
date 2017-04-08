%macro UnsetFlags 0
    mov ax, 0
    push ax
    popf
%endmacro

%macro SetFlags 0
    mov ax, 0ffffh
    push ax
    popf
%endmacro

%macro GetFlags 0
    pushf
    pop ax
%endmacro

%define Mask_CF   0000000000000001b
%define Mask_PF   0000000000000100b
%define Mask_AF   0000000000010000b
%define Mask_ZF   0000000001000000b
%define Mask_SF   0000000010000000b
%define Mask_OF   0000100000000000b
