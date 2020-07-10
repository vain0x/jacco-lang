// Library wrappers and main function.

#include <stdio.h>
#include <stdlib.h>

void cc_main();

typedef int i32;
typedef unsigned long long usize;
typedef unsigned char c8;

void print_s(c8 const* value) { fputs((char const *)value, stdout); }

void print_d(i32 value) { printf("%d", value); }

void eprint_s(c8 const* s) { fputs((char const *)s, stderr); }

int main(int argc, char **argv) {
    cc_main((usize)argc, (c8 const* const *)argv);
    return 0;
}
