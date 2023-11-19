#include <stdlib.h>

int main(void) {
  int *px = malloc(sizeof(int));
  *px = 42;
  int x = *px;
  return x;
}
