// adapted from cheri-examples

/***
 * Testing to see what the representable
 * lengths function returns for different
 * values.
 */

#include <cheriintrin.h>
#include <stdint.h>
//#include <stdio.h>

int main() {
    int x;
    for (uint64_t length = 0; length <= (4096 * 20); length = 2 *length + 31) {
       x = cheri_representable_length(length);
    }
    return x;
}
