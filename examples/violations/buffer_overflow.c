#include <stdlib.h>

int main() {
    int *x = malloc(sizeof(int) * 3);
    for(int i = 0; i <= 3; i++) {
        x[i] = i;
    }
    return 0;
}
