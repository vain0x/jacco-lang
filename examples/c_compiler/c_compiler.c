// Library wrappers and main function.

#include <stdio.h>
#include <stdlib.h>

void cc_main();

int jacco_std_io_print(unsigned char const *s) {
    return fputs((char const *)s, stdout) != EOF;
}

int jacco_std_io_eprint(unsigned char const *s) {
    return fputs((char const *)s, stderr) != EOF;
}

void jacco_std_process_exit(int code) {
    exit(code);
}

int main(int argc, char **argv) {
    cc_main((unsigned long long)argc, (unsigned char const *const *)argv);
    return 0;
}
