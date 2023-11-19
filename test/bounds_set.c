#include <stdlib.h>

int main() {

  int *p = malloc(sizeof(int)); 
  *p = 42;
  for (int i = sizeof(int); i>=0; i--) {
    p = __builtin_cheri_bounds_set(p, i);
  } 

  int *q = malloc(sizeof(int));
  q = __builtin_cheri_bounds_set(q, sizeof(int)+1);
  for (int i = sizeof(int); i>=0; i--) {
    p = __builtin_cheri_bounds_set(p, i);
  }
  
  return 0; 
} 
