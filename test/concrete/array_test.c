#include <stdlib.h>

/* This one tests alignment. */
int main() {
  int *p = malloc(4*sizeof(int)); 
  *p = 1;
  int *q = p+1;
  int *r = __builtin_align_up(q,5);
  *q = 2;

  int x = *p;
  return x;
}
