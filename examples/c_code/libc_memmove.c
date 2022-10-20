#include <stdlib.h>
#include <string.h>
#include <cheriintrin.h>
#include "cheri_c_test.h"
/*
int foo() {
    char *pointees = malloc(sizeof(char) * 10);
    pointees[0] = '0';
    pointees[1] = '1';
    pointees[2] = '2';
    pointees[3] = '3';
    pointees[4] = '4';
    pointees[5] = '5';
    pointees[6] = '6';
    pointees[7] = '7';
    pointees[8] = '8';
    pointees[9] = '9';

    char **buffer = malloc(sizeof(char *) * 10);
    char **buffer1 = malloc(sizeof(char *) * 10);
    char **bufferx = malloc(sizeof(char *) * 11);
    char **buffer2 = malloc(sizeof(char *) * 10);
    buffer[0] = pointees;
    buffer[1] = pointees + 1;
    buffer[2] = pointees + 2;
    buffer[3] = pointees + 3;
    buffer[4] = pointees + 4;
    buffer[5] = pointees + 5;
    buffer[6] = pointees + 6;
    buffer[7] = pointees + 7;
    buffer[8] = pointees + 8;
    buffer[9] = pointees + 9;

    // memcpy(buffer, buffer + 2, 320 - 2 * sizeof(char *));
    memcpy(buffer1, buffer, sizeof(char *) * 10);
    memcpy((char *) bufferx + 1, buffer, sizeof(char *) * 10);
    memcpy(buffer2, (char *) bufferx + 1, sizeof(char *) * 10);
    for(int i = 0; i < 10; i++)
    {
        assert(cheri_tag_get(buffer1[i]));
        assert(!cheri_tag_get(buffer2[i]));
        assert(buffer1[i] == buffer2[i]);
    }
    return 0;
}
*/

int main() {
    char *pointees = malloc(sizeof(char) * 10);
    pointees[0] = '0';
    pointees[1] = '1';
    pointees[2] = '2';
    pointees[3] = '3';
    pointees[4] = '4';
    pointees[5] = '5';
    pointees[6] = '6';
    pointees[7] = '7';
    pointees[8] = '8';
    pointees[9] = '9';

    char **buffer = malloc(sizeof(char *) * 10);

    buffer[0] = pointees;
    buffer[1] = pointees + 1;
    buffer[2] = pointees + 2;
    buffer[3] = pointees + 3;
    buffer[4] = pointees + 4;
    buffer[5] = pointees + 5;
    buffer[6] = pointees + 6;
    buffer[7] = pointees + 7;
    buffer[8] = pointees + 8;
    buffer[9] = pointees + 9;

    memmove(buffer, buffer + 2, sizeof(char *) * 10 - 2 * sizeof(char *));

    for(int i = 0; i <8; i++)
    {
        assert_eq(*buffer[i], '0' + i + 2);
    }
    return 0;
}
