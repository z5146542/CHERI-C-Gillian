#include <stdlib.h>
#include <string.h>

int main(void) {
    int *n = calloc(sizeof(int), 1);
    int **a = malloc(sizeof(int *));
    *a = n;
    int **b = malloc(sizeof(int *) * 2);
    int **c = malloc(sizeof(int *));
    memcpy((char *) b + 1, a, sizeof(int *));
    memcpy(c, (char *) b + 1, sizeof(int *));
    int x = **c;
    return 0;
}
