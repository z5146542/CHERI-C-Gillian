#include <stdlib.h>

int main() {
    int *x = malloc(sizeof(int) * 10);
    for(int i = 0; i < 10; i++) x[i] = 0;
    x = x + 2;
    free(x);
    return 0;
}
