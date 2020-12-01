// ジャッコ言語の構文的操作を扱うライブラリ

#include <stdio.h>
#include <stdlib.h>

typedef unsigned char u8;
typedef unsigned long long usize;

extern int libjacco_syntax_tests();

void eprint_s(u8 const* value) {
    fprintf(stderr, "%s", value);
}

void eprint_t(u8 const* value, usize len) {
    fprintf(stderr, "%.*s", (int)len, value);
}

void eprint_d(int value) {
    fprintf(stderr, "%d", value);
}

int main() {
    return libjacco_syntax_tests();
}
