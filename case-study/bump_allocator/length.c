#include <assert.h>
#include <stdlib.h>
#include <limits.h>
#include "bump_alloc.h"

void minimum_length() {
  int x = nondet_int();
  __ESBMC_assume(x>=0);
  __ESBMC_assume(x<INT_MAX);
  init_alloc(x);
  char *p = bump_alloc(x);
  assert(__builtin_cheri_length_get(p) >= x);
}

int main() {
  minimum_length();
  return 0;
}
