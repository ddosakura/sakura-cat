test.ll: export ROOT=/home/moyinzi/projects/gPro/sakura-cat/tm-test/test/
test.ll: export SKRBIN=$(ROOT)bin
test.ll: export SKRCACHE=$(ROOT)cache
test.ll: export SKRPATH=$(ROOT)src
test.ll: export SKRMOD=$(ROOT)src/test1/skr.mod
test.ll: main.go test/src/test1/*.skr grammar/skr.tm
	# ./build.sh
	@echo ENV[ROOT]     = $(ROOT)
	@echo ENV[SKRBIN]   = $(SKRBIN)
	@echo ENV[SKRCACHE] = $(SKRCACHE)
	@echo ENV[SKRPATH]  = $(SKRPATH)
	@echo ENV[SKRMOD]   = $(SKRMOD)
	@go run main.go

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
	lib64/build/entry.o \
	test.o; chmod +x test.out
test.a.out: test.o
	ld -s -o test.a.out \
	test.o \
	lib64/build/skr.a \
	lib64/build/entry.o; chmod +x test.out
#test.so.out: lib64/build/skr.so test.o
#	ld -s -o test.so.out \
#	-dynamic-linker lib64/build/skr.so \
#	test.o \
#	lib64/build/entry.o; chmod +x test.so.out

clean:
	rm *.bc *.ll *.o *.out *.s *.lst *.tmp.asm *.so *.a *.dump

run: test.ll
	# lli -load path-to-so -entry-function _start test.ll
	# lli -load lib64/build/io.o -relocation-model=static -entry-function _start test.ll
	lli -load lib64/build/skr.so test.ll
