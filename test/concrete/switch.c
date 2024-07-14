#include <assert.h>

int test(int x) {
  switch(x) { 
  case 5:
    return 5;
  default:
    return 7;
  }
} 

int main() {
  int y = test(5);
  assert(y==5);
  int z = test(1);
  assert(z==7);
  return 0;
}
