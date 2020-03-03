[bits 64]
global _start:function

_start:
    mov rax, 1
    mov rdi, 1
    mov rsi, hello_str
    mov rdx, hello_len
    syscall

    neg rax
    mov rdi, rax
    mov rax, 60
    syscall

hello_str: db "Hello, World!", 10, 0
hello_len equ ($ - hello_str)
