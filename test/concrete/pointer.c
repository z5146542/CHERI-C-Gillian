#include <stdlib.h>

int main(void) {
  int *px = malloc(sizeof(int));
  int *py = malloc(sizeof(int));
  *px = 42;
  int x = *px;
  py = px;
  int y = *py;
  return y;
}
