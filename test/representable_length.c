#include <cheriintrin.h>
#include <stdlib.h>
#include <stdint.h>
#include <assert.h>

int main() {
  
  uint64_t x0 = 0;
  uint64_t x1 = 1;
  uint64_t x2 = 16384; /* 2^14 */
  uint64_t x3 = 16385;
  uint64_t x4 = 32768; /* 2^15 */
  uint64_t x5 = 510510; 
  uint64_t x6 = 1125899906842624; /* 2^50 */
  uint64_t x7 = 1125899906842625; /* 2^50+1 */
  uint64_t x8 = 4503599627370496; /* 2^52 */ 
  uint64_t x9 = 18446744073709551615ULL; 
  
  /* y0-y2 should not be rounded */
  uint64_t y0 = __builtin_cheri_round_representable_length(x0);
  uint64_t y1 = __builtin_cheri_round_representable_length(x1);
  uint64_t y2 = __builtin_cheri_round_representable_length(x2);
  assert(x0==y0);
  assert(x1==y1);
  assert(x2==y2);
  /* y3-y6 are subject to rounding v1 */
  uint64_t y3 = __builtin_cheri_round_representable_length(x3);
  assert (y3==16392);
  uint64_t y4 = __builtin_cheri_round_representable_length(x4);
  assert(y4==32768);
  uint64_t y5 = __builtin_cheri_round_representable_length(x5);
  assert(y5==510592);
  uint64_t y6 = __builtin_cheri_round_representable_length(x6);
  assert(y6==1125899906842624);
  /* y7-y8 are subject to rounding v2 -- these are currently experiencing overflow issues */
  uint64_t y7 = __builtin_cheri_round_representable_length(x7);
  uint64_t y8 = __builtin_cheri_round_representable_length(x8);
  return 0;
}
