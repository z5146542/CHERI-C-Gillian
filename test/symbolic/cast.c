#include <stdint.h>
#include <assert.h>

int main() {
  int32_t x = nondet_int(); 
  __ESBMC_assume(x>2);
  int16_t y = x;  
  assert(y>2);
  return 0;
}
