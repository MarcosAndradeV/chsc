#ifndef CHS_LIBC
#define CHS_LIBC

extern fn puts(ptr);
extern fn printf(ptr, ...) -> i32;
extern fn malloc(word) -> ptr;
extern fn free(ptr);

#endif
