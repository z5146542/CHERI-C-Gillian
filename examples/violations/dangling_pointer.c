#include <stdlib.h>

int main() {
    int *x = malloc(sizeof(int));
    *x = 420;
    int *y = x;
    free(x);
    int z = *y;
    return 0;
}
