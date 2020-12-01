// ジャッコ言語のコードをテストするときのライブラリ。

#include <stdio.h>

void abort();

void assert(int cond) {
    if (!cond) {
        fprintf(stderr, "[ERROR] Assertion violated\n");
        abort();
    }

    // fprintf(stderr, "[TRACE] Assertion pass\n");
}

#include <stdlib.h>
#include <string.h>

void print_sss(char const* s1, char const* s2, char const *s3) {
    printf("%s%s%s", s1, s2, s3);
}

void print_sds(char const* s1, int d2, char const *s3) {
    printf("%s%d%s", s1, d2, s3);
}
