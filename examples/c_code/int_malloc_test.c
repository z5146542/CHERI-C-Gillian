#include <stdlib.h>

int main() {
    int *x = malloc(sizeof(int) * 3);
    x[0] = 7;
    x[1] = 14;
    x[2] = 21;
    int a = x[0];
    int b = x[1];
    int c = x[2];
    return 0;
}
