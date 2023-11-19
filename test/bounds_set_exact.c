#include <stdlib.h>

int main() {
  int *p = malloc(2*sizeof(int));
  __builtin_cheri_bounds_set_exact(p, sizeof(int));
  return 0; 
} 
