extern main
global _start
_start:
    call main       ; define i32 @main()
	mov	ebx, eax
	mov	eax, 1		; sys_exit
	int	0x80

global puts
puts:
    add esp, 4
    ;pop eax            ; ret 后的下一条指令 
    pop ecx             ; 参数1
    pop edx             ; 参数2
    ;push edx
    ;push ecx
    ;push eax
    sub esp, 12

	;mov	edx, ebx    ; len
	;mov	ecx, eax    ; str
	mov	ebx, 1
	mov	eax, 4          ; sys_write
	int	0x80
    ret
