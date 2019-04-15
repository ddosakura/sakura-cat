extern main
global _start
_start:
    call main       ; define void @main()
    mov rdi, 0      ; error_code
    mov rax, 60     ; sys_exit
    syscall