#include <stdlib.h>
#include <assert.h>

int gx = 5; 

int fac(int n) {
  int res = 1;
  int i = 0;
  while (i<n) {
    i++; res = res * i;
  }
  return res;
}

int main() {
  int lx = 5; 
  assert(lx==gx);
  assert(fac(lx)==fac(gx));
  return 0;
}
