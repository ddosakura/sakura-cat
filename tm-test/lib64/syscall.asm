;用户模式的系统调用依次传递的寄存器为: rdi,rsi,rdx,rcx,r8和r9；

global _skr_puts
_skr_puts:
    mov rdx, rsi    ; lenght of msg string
    mov rsi, rdi    ; addr of msg string
    mov rdi, 1      ; fd
    mov rax, 1      ; sys_write
    syscall
    ret
