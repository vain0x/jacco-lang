CFLAGS=-std=c11 -Wall -Wno-unused-variable -Wno-unused-but-set-variable -static

# 既定のターゲットは一番上に書く。
c_compiler: c_compiler.g.c c_compiler.c

c_compiler.g.c: c_compiler.jacco
	RUST_LOG=warn ../../target/debug/jacco build c_compiler.jacco >c_compiler.g.c

test: c_compiler
	./test

# rm -f c_compiler *.jacco.c *.o *.out *.s *~ tmp*
clean:
	git clean -dfX .

.PHONY: clean test
