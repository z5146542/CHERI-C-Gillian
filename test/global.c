#include <assert.h>

int x = 1;
int y;
int z;

int main() {
  assert(x==1);
  z = x+1;
  assert(z==2);
  z = z*z;
  assert(z==4);
  return 0;
};	  
