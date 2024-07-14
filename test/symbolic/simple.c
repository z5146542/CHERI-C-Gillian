#include <stdlib.h>
#include <assert.h>

int main() {

  int la = nondet_int();
  int lb = nondet_int();
  __ESBMC_assume(la==lb);
  assert(la!=lb);

  return 0;
}
