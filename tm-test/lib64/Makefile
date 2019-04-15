syscall.o: syscall.asm
	nasm -f elf64 -o syscall.o -l syscall.lst syscall.asm
entry.o: entry.asm
	nasm -f elf64 -o entry.o -l entry.lst entry.asm

io.ll: io/io.go
	go run io/io.go
io.bc: io.ll
	llvm-as io.ll -o io.bc
io.o: io.bc
	llc io.bc -o io.o -filetype=obj

test.ll: test/test.go
	go run test/test.go
test.bc: test.ll
	llvm-as test.ll -o test.bc
test.o: test.bc
	llc test.bc -o test.o -filetype=obj

test.out: entry.o syscall.o io.o test.o
	ld -s -o test.out \
	entry.o \
	syscall.o \
	io.o \
	test.o; chmod +x test.out

test.tmp.asm: test.bc
	llc test.bc -o test.tmp.asm --x86-asm-syntax=intel
io.tmp.asm: io.bc
	llc io.bc -o io.tmp.asm --x86-asm-syntax=intel
debug:	test.tmp.asm io.tmp.asm
	objdump test.out -M intel -D > objdump.tmp.asm

seea: build/skr.a
	ar -t build/skr.a
seeso: build/skr.so
	ldd build/skr.so
test.a.out: build/skr.a test.o
	ld -s -o test.a.out \
	test.o \
	build/skr.a \
	build/entry.o; chmod +x test.a.out
test.so.out: build/skr.so test.o
	ld -s -o test.so.out \
	-dynamic-linker build/skr.so \
	test.o \
	build/entry.o; chmod +x test.so.out

clean:
	rm *.bc *.ll *.o *.out *.s *.lst *.tmp.asm *.so *.a *.dump
build: syscall.o io.o entry.o
	rm -rf ./build
	mkdir ./build
	mv entry.o ./build/
	ar rcs skr.a *.o
	# ar -x skr.a # 解包
	#clang -shared *.o -o skr.so
	clang -fPIC -shared *.o -o skr.so
	mv *.a ./build/
	mv *.o ./build/
	mv *.so ./build/
	make clean

# -dynamic-linker
# -relocation-model=static
# -relocation-model=pic
