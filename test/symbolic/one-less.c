#include <stdlib.h>
#include <assert.h>

int ga;
int gb;

int main() {

  ga = nondet_int();
  gb = nondet_int();
  __ESBMC_assume(ga==gb-1);
  assert(ga<gb);

  int la = nondet_int();
  int lb = nondet_int();
  __ESBMC_assume(la==lb-1);
  assert(la<lb);

  return 0;
}
