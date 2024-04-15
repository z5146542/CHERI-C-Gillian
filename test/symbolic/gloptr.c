#include <stdlib.h>
#include <assert.h>

int* px;
int* py;

int main() {
  px = malloc(sizeof(int));
  py = malloc(sizeof(int));
  *px = nondet_int();
  *py = nondet_int();
  __ESBMC_assume(*px<2);
  __ESBMC_assume(*py>2);
  assert(*px<*py);
  return 0;
}
