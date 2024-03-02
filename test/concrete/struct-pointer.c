#include <stdlib.h>
#include <assert.h>

struct TestStruct {
  int first;
  int second;
};

int main(void) {
  struct TestStruct *px = malloc(sizeof(struct TestStruct));
  px->first = 42;
  int x = px->first;
  assert(x==42);
  return 0;
}
