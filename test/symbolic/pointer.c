#include <stdlib.h>
#include <assert.h>

int *px;

int main(void) {
  px = malloc(sizeof(int));
  int *py = malloc(sizeof(int));
  *px = 42;
  int x = *px;
  py = px;
  int y = *py;
  assert(*px==42);
  assert(*py==42);
  return y;
}
