#include <assert.h>

int x = 5;

int main() {
  int t = 0;
  int i = 0;
  while (i < x) {
    t += i;
    i++;
  }
  assert(t==10);
  return 0;
} 
