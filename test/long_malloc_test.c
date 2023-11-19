#include <stdlib.h>

int main() {
    long *x = malloc(sizeof(long) * 3);
    x[0] = 256;
    x[1] = 65536;
    x[2] = 2147483648;
    long a = x[0];
    long b = x[1];
    long c = x[2];
    return 0;
}
