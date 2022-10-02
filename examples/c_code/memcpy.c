#include <stdlib.h>
#include <string.h>

void my_memcpy(void *dest, void *src, size_t n) {
    char *csrc = (char *) src;
    char *cdst = (char *) dest;
    for(size_t i = 0; i < n; i++) {
        cdst[i] = csrc[i];
    }
}

void my_memmove(void *dest, void *src, size_t n) {
    char *temp = malloc(sizeof(char) * n);
    my_memcpy(temp, src, n);
    my_memcpy(dest, temp, n);
    free(temp);
}

int foo() {
    // this should fail.
    int **src = malloc(sizeof(int *) * 2);
    int *src1 = malloc(sizeof(int));
    int *src2 = malloc(sizeof(int));
    src[0] = src1;
    src[1] = src2;
    // this should fail.
    // memcpy(src, src, sizeof(int *) * 2);
    // however, this should work.
    memmove(src, src, sizeof(int *) * 2);
    return 0;
}

int bar() {
    // Here we demonstrate the difference b/w user-defined memcpy and i__memcpy
    int **src = malloc(sizeof(int *) * 2);
    int *src1 = malloc(sizeof(int));
    int *src2 = malloc(sizeof(int));
    src[0] = src1;
    src[1] = src2;
    int **dt1 = malloc(sizeof(int *) * 2);
    int **dt2 = malloc(sizeof(int *) * 2);
    my_memmove(dt1, src, sizeof(int *) * 2);
    memmove(dt2, src, sizeof(int *) * 2);
    int *dt11 = dt1[0];
    int *dt12 = dt1[1];
    int *dt21 = dt2[0];
    int *dt22 = dt2[1];
    return 0;
}

int baz() {
    int **x = malloc(sizeof(int *));
    int *x1 = malloc(sizeof(int));
    *x = x1;
    int **y = malloc(sizeof(int *));
    my_memcpy(y, x, sizeof(int *));
    int *a = *x;
    int *b = *y;
    int is_same = a == b;
    // next
    int *z = malloc(sizeof(int) * 3);
    z[0] = 7;
    z[1] = 14;
    z[2] = 21;
    int *w = malloc(sizeof(int) * 3);
    my_memcpy(w, z, sizeof(int) * 3);
    int is_same_2 = 1;
    if(z[0] != w[0]) is_same_2 = 0;
    if(z[1] != w[1]) is_same_2 = 0;
    if(z[2] != w[2]) is_same_2 = 0;
    return 0;
}

int main() {
    int *x = malloc(sizeof(int) * 3);
    x[0] = 7;
    x[1] = 14;
    x[2] = 21;
    int *y = malloc(sizeof(int) * 3);
    int *z = malloc(sizeof(int) * 3);
    my_memcpy(y, x, sizeof(int) * 3);
    memcpy(z, x, sizeof(int) * 3);
    int y0 = y[0];
    int y1 = y[1];
    int y2 = y[2];
    int z0 = z[0];
    int z1 = z[1];
    int z2 = z[2];
    return 0;
}
