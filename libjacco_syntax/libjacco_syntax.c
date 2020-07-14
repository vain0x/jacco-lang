// ジャッコ言語の構文的操作を扱うライブラリ

#include <stdio.h>
#include <stdlib.h>

typedef unsigned char u8;

extern int libjacco_syntax_tests();

void eprint_s(u8 const* value) {
    fprintf(stderr, "%s", value);
}

void eprint_d(int value) {
    fprintf(stderr, "%d", value);
}

int main() {
    return libjacco_syntax_tests();
}
