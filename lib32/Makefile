syscall.o: syscall.asm
	nasm -f elf -o syscall.o -l syscall.lst syscall.asm

io.ll: io/io.go
	go run io/io.go
io.bc: io.ll
	llvm-as io.ll -o io.bc
io.o: io.bc
	llc io.bc -o io.o -filetype=obj -march=x86

test.ll: test/test.go
	go run test/test.go
test.bc: test.ll
	llvm-as test.ll -o test.bc
test.o: test.bc
	llc test.bc -o test.o -filetype=obj -march=x86

test.out: syscall.o io.o test.o
	ld -s -m elf_i386 -o test.out \
    syscall.o \
    io.o \
    test.o; chmod +x test.out

test.tmp.asm: test.bc
	llc test.bc -o test.tmp.asm --x86-asm-syntax=intel -march=x86
io.tmp.asm: io.bc
	llc io.bc -o io.tmp.asm --x86-asm-syntax=intel -march=x86
debug:	test.tmp.asm io.tmp.asm
	objdump test.out -M i386,intel -D > objdump.tmp.asm

clean:
	rm *.bc *.ll *.o *.out *.s *.lst *.tmp.asm
