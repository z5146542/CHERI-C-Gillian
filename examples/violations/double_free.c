#include <stdlib.h>

int main() {
    int *x = malloc(sizeof(int));
    *x = 3;
    free(x);    
    free(x);
    return 0;
}
