// Library wrappers and main function.

#include <stdio.h>

void cc_main();

int jacco_std_io_write(unsigned char const *s) {
    return fputs((char const *)s, stdout) != EOF;
}

int main() {
    cc_main();
    return 0;
}
