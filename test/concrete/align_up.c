#include <cheriintrin.h>
#include <stdlib.h>
#include <assert.h>
#include <stdalign.h>

int main() {
  int *p0 = malloc(sizeof(int)*4);
  int *p1 = p0+1;
  int *p2 = p0+2;
  int *p3 = p0+3;

  p1 = __builtin_align_up(p1, 7);
  assert(cheri_tag_get(p1)==1);
  p2 = __builtin_align_up(p2, 100);
  assert(cheri_tag_get(p2)==0);

  return 0;
}
