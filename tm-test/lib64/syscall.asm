;用户模式的系统调用依次传递的寄存器为: rdi,rsi,rdx,rcx,r8和r9；

extern main
global _start
_start:
    call main       ; define void @main()
    mov rdi, 0      ; error_code
    mov rax, 60     ; sys_exit
    syscall

global puts
puts:
    mov rdx, rsi    ; lenght of msg string
    mov rsi, rdi    ; addr of msg string
    mov rdi, 1      ; fd
    mov rax, 1      ; sys_write
    syscall
    ret
