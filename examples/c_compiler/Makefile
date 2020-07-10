CFLAGS=-std=c11 -Wall -Wno-unused-variable -Wno-unused-but-set-variable -static

c_compiler.jacco.c:
	RUST_LOG=warn ../../target/debug/jacco build c_compiler.jacco >c_compiler.jacco.c

c_compiler: c_compiler.jacco.c c_compiler.c

test: c_compiler
	./test

# rm -f c_compiler *.jacco.c *.o *.out *.s *~ tmp*
clean:
	git clean -dfX .

.PHONY: clean test
