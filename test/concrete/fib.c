#include <stdlib.h>
#include <assert.h>

int *ptr;
int x = 10;

int fib(int n) {
  assert(n>=0);
  if (n<=1) return n;
  else return fib(n-1)+fib(n-2);
}

int main() {
  ptr = malloc(sizeof(int));
  int r = fib(x);
  *ptr = r;
  return 0;
}
