test.ll: main.go
	# go run main.go
	./build.sh

test.o3.ll: test.ll
	opt -O3 -S -o test.o3.ll test.ll

test.bc: test.o3.ll
	llvm-as test.o3.ll -o test.bc
test.o: test.bc
	llc test.bc -o test.o -filetype=obj
test.out: test.o
	ld -s -o test.out \
    lib64/build/syscall.o \
    lib64/build/io.o \
    test.o; chmod +x test.out

clean:
	rm *.bc *.ll *.o *.out *.s *.lst *.tmp.asm
