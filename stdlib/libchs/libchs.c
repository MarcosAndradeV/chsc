#define LIB_CHS
#include <stdio.h>

void put_int(int x) { printf("%d", x); }
void putln_int(int x) { printf("%d\n", x); }

void put_char(char x) { printf("%c", x); }
void putln_char(char x) { printf("%c\n", x); }

void put_string(const char* x) { printf("%s", x); }
void putln_string(const char* x) { printf("%s\n", x); }

void put_bool(int x) { x ? printf("true") : printf("false"); }
void putln_bool(int x){ x ? printf("true\n") : printf("false\n"); }


// void put_int(uint32_t input) {
//     if(input == 0) {
//         write(STDOUT_FILENO, "0", 1);
//         return;
//     }
//     char rev_str[11];
//     char str[11];
//     uint32_t len = 0;

//     while (input) {
//         char digit = (input % 10) + 48;
//         input = input / 10;
//         rev_str[len++] = digit;
//     }

//     for (uint32_t i = 1; i < len + 1; i++) {
//         str[i-1] = rev_str[len - i];
//     }
//     str[len] = '\0';

//     write(STDOUT_FILENO, str, len);
// }
