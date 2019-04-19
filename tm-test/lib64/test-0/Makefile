test.o3.ll: test.ll
	opt -O3 -S -o test.o3.ll test.ll

test.bc: test.o3.ll
	llvm-as test.o3.ll -o test.bc
test.o: test.bc
	llc test.bc -o test.o -filetype=obj
test.out: test.o
	ld -s -o test.out \
	../build/syscall.o \
	../build/io.o \
	../build/entry.o \
	test.o; chmod +x test.out
test.a.out: test.o
	ld -s -o test.a.out \
	test.o \
	../build/skr.a \
	../build/entry.o; chmod +x test.out

clean:
	rm *.bc *.ll *.o *.out *.s *.lst *.tmp.asm *.so *.a *.dump

run: test.ll
	lli -load ../build/skr.so test.ll
