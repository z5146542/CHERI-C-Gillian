#include <stdlib.h>
#include <assert.h>

struct EmptyStruct {
};

struct UniStruct {
  int first;
};

struct BiStruct {
  int first;
  int second;
};

struct TriStruct {
  int first;
  int second;
  int third;
};

struct MixedStruct {
  int x;
  char *y;
};

int main() {
  
  struct EmptyStruct e;

  struct UniStruct *u;
  u->first = 1;
  int one = u->first;
  assert(one==1);

  struct BiStruct b;
  b.first = 1;
  b.second = 2;
  assert(b.first+b.second==3);
  int three = b.first + b.second;
  assert(three==3);

  struct TriStruct *t;
  t->first = 1;
  t->second = 2;
  t->third = 3;
  int six = t->first * t->second * t->third;
  assert(six=6);

  int four = b.second * t->second;
  assert(four==4);

  struct MixedStruct m;
  m.x = 0;
  char *z;
  z  = malloc(sizeof(char));
  *z = 'z';
  m.y = z;
  assert(z==m.y);
  assert(*z==*(m.y));

  return 0;
}
