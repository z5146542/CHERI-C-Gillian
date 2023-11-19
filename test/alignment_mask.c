#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

int main() {

  /* x0-x2 are exactly representable */ 
  uint64_t x0 = 0;
  uint64_t x1 = 1;
  uint64_t x2 = 16384;
  uint64_t x3 = 16385; /* 2^14+1 */
  uint64_t x4 = 32767; /* 2^15-1 */
  uint64_t x5 = 32769;
 
  uint64_t y0 = __builtin_cheri_representable_alignment_mask(x0);
  assert(y0==18446744073709551615ULL);
  uint64_t y1 = __builtin_cheri_representable_alignment_mask(x1);
  assert(y1==18446744073709551615ULL);
  uint64_t y2 = __builtin_cheri_representable_alignment_mask(x2);
  assert(y2==18446744073709551615ULL);
  uint64_t y3 = __builtin_cheri_representable_alignment_mask(x3);
  assert(y3==18446744073709551608ULL);
  uint64_t y4 = __builtin_cheri_representable_alignment_mask(x4);
  assert(y4==18446744073709551608ULL);
  uint64_t y5 = __builtin_cheri_representable_alignment_mask(x5);
  assert(y5==18446744073709551600ULL);

  return 0;
}
