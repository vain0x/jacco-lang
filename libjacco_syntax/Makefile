# これの置かれたディレクトリで `make` コマンドを使う。

SRCS=libjacco_syntax.c libjacco_syntax.g.c
CFLAGS=-std=c11 -Wall -Wno-unused-variable -Wno-unused-but-set-variable -static

test: libjacco_syntax
	./libjacco_syntax.out

libjacco_syntax: ${SRCS}
	gcc ${CFLAGS} ${SRCS} -o libjacco_syntax.out

libjacco_syntax.g.c: libjacco_syntax.jacco
	RUST_LOG=warn ../target/debug/jacco build libjacco_syntax.jacco >libjacco_syntax.g.c

clean:
	git clean -dfX .

.PHONY: clean test
