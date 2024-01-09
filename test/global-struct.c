#include <stdlib.h>
#include <assert.h>

struct TestStruct {
  int elem;
  int elem2;
};

struct TestStruct *ptr;

int main() {
  ptr->elem = 42;
  int x = ptr->elem;
  assert(x==42);
  return 0;
}
