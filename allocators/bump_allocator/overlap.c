#include <assert.h>
#include <stdlib.h>
#include <limits.h>

/* allocate a nondet number of capabilities, each of nondet size
 * we should never have allocated in a region that was already given up */

void overlap() {
  int to_alloc = nondet_int();
  __ESBMC_assume(to_alloc>=2);
  __ESBMC_assume(to_alloc<10);

  int x = nondet_int();
  int y = nondet_int();
  //__ESBMC_assume(x>=0);
  //__ESBMC_assume(y>=0);
  //__ESBMC_assume(y<to_alloc-1);
  __ESBMC_assume(x!=y);

  __ESBMC_assume(x >= 0 && x < to_alloc);
  __ESBMC_assume(y >= 0 && y < to_alloc);

  //int xl;
  //int yl;
  char *px;
  char *py;

  for (int i = 0; i < to_alloc; i++) {
    int length = nondet_int();
    __ESBMC_assume(length>=0);
    __ESBMC_assume(length<INT_MAX);
    char *p = malloc(length);
    if (i==y) {
       py = p;
    }
    if (i==x) {
       px = p;
    }
  }

  char* lower;
  char* higher;
  //int len;
  int test = __builtin_cheri_address_get(px) < __builtin_cheri_address_get(py);
  if (test) { higher = py; lower = px; }
  else { higher = px; lower = py; }
  assert(__builtin_cheri_address_get(lower)+__builtin_cheri_length_get(lower) < __builtin_cheri_address_get(higher));
}

void simple() {

  int x = nondet_int();
  int y = nondet_int();
  __ESBMC_assume(x>=0);
  __ESBMC_assume(y>=0);
  __ESBMC_assume(y<INT_MAX);
  __ESBMC_assume(x<INT_MAX);

  char *px = malloc(x);
  char *py = malloc(y);

  int test = __builtin_cheri_address_get(px) < __builtin_cheri_address_get(py);

  char* lower;
  char* higher;
  if (test) { higher = py; lower = px; }
  else { higher = px; lower = py; }
  assert(__builtin_cheri_address_get(lower)+__builtin_cheri_length_get(lower) < __builtin_cheri_address_get(higher));

}

int main() {
  overlap();
  //simple();
  return 0;
}
