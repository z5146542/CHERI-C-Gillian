#include <stdlib.h>

int main(void) {
    int *x = malloc(sizeof(int) * 10);
    int *y = malloc(sizeof(int) * 10);
    for(int i = 0; i < 10; i++) {
        x[i] = i;
        y[i] = x[i] + 1;
    }
    free(y);
    free(x);
    return 0;
}
