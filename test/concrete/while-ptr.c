#include <stdlib.h>

int main() {
  int *px = malloc(sizeof(int));
  int *py = px;
  int i = 0;
  while (px == py) {
    i++;
    py = malloc(sizeof(int));
  }
  return 0;
}
