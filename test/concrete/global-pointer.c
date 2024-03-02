#include <stdlib.h>
#include <assert.h>

int *px = NULL;
int *py;
int gx;

int main() {
  px = malloc(sizeof(int));
  *px = 42; 
  py = px;
  int lx = *px;
  gx = *py;
  assert(lx==gx);
  return 0;
}
