#include <stdlib.h>

int main(void) {
  char *pc = malloc(sizeof(char));
  int *px = malloc(2*sizeof(int));
  int *py = malloc(sizeof(int));
  *px = 42;
  int *pz = px+1;
  int *pw = pz-1;
  *pw = 43;
  int x = pz-px;
  py = px;
  int y = *py;
  return y;
}
