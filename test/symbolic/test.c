#include <stdlib.h>
#include <assert.h>

int a;
int b;

int main() {


  a = nondet_int();
  b = nondet_int();
  __ESBMC_assume(a>=0);
  __ESBMC_assume(a<b);

  assert(a*a<b*b);

  return 0;
}
