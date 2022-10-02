#include <stdlib.h>

int main() {
    char *x = malloc(sizeof(char) * 3);
    for(int i = 0; i < 3; i++) {
        x[i] = 7 * (i + 1);
    }
    char a = x[0];
    char b = x[1];
    char c = x[2];
    free(x);
    
    char *x1 = malloc(sizeof(char));
    char *y1 = malloc(sizeof(char));
    char *z1 = malloc(sizeof(char));
    *x1 = 7;
    *y1 = 14;
    *z1 = 21;
    char a1 = *x1;
    char b1 = *y1;
    char c1 = *z1;
    free(z1);
    free(y1);
    free(x1);
    
    char *x2 = malloc(sizeof(char) * 3);
    int n = 7;
    x2[0] = n;
    x2[1] = 14;
    x2[2] = 21;
    char a2 = x2[0];
    char b2 = x2[1];
    char c2 = x2[2];
    // char d = x[3];
    free(x2);
    // char e = x[2];
    return 0;
}

int foo() {
    int *x = malloc(sizeof(int) * 3);
    x[0] = 7;
    x[1] = 14;
    x[2] = 21;
    int a = x[0];
    int b = x[1];
    int c = x[2];
    return 0;
}

