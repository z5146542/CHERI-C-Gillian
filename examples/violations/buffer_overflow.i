# 0 "../CHERI-C-Gillian/examples/violations/buffer_overflow.c"
# 0 "<built-in>"
# 0 "<command-line>"
# 1 "/usr/include/stdc-predef.h" 1 3 4
# 0 "<command-line>" 2
# 1 "../CHERI-C-Gillian/examples/violations/buffer_overflow.c"
# 1 "/home/simark/Documents/Gillian/_esy/default/store/i/gillian_platform-b90f49c9/share/include/stdlib.h" 1





typedef unsigned char uint8_t;

typedef unsigned long size_t;

void *malloc(size_t size);

void *calloc(size_t num, size_t size);

void free(void *ptr);

void *memcpy(void *destination, const void *source, size_t num);

void *memmove(void *destination, const void *source, size_t num);

void *memset(void *ptr, int value, size_t num);

int rand(void);


void qsort(void *base, size_t num, size_t size,
           int (*comparator)(const void *, const void *));
# 2 "../CHERI-C-Gillian/examples/violations/buffer_overflow.c" 2

int main() {
    int *x = malloc(sizeof(int) * 3);
    for(int i = 0; i <= 3; i++) {
        x[i] = i;
    }
    return 0;
}
