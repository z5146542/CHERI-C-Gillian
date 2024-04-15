#include <stdlib.h>
#include <assert.h>
#include <stdint.h>


int main() {

  int a;
  int b;
  a = nondet_int();
  b = nondet_int();
  __ESBMC_assume(a>=0);
  __ESBMC_assume(a<b);
  assert(a*a<b*b);

  int16_t c;
  int16_t d;
  c = nondet_short();
  d = nondet_short();
  __ESBMC_assume(c>=0);
  __ESBMC_assume(c<d);
  assert(c*c<d*d);

  return 0;
}
