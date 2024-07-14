#include <stdlib.h>

int main() {
    int *x1 = malloc(sizeof(int));
    int *x2 = malloc(sizeof(int));
    int **x = malloc(sizeof(int *) * 2);
    x[0] = x1;
    x[1] = x2;
    int *y1 = x[0];
    int *y2 = x[1];
    // Here, we get capability fragments. 
    char c1 = *((char *) x);
    char c2 = *((char *) x + 1);
    return 0;
}
