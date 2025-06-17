#ifndef CHS_LIBC
#define CHS_LIBC

extern fn puts(str);
extern fn printf(str, ...);
extern fn malloc(size);
extern fn free(ptr);

#endif
