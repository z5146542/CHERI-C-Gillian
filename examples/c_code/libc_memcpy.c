/*-
 * Copyright (c) 2012-2015 David Chisnall
 * All rights reserved.
 *
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory under DARPA/AFRL contract (FA8750-10-C-0237)
 * ("CTSRD"), as part of the DARPA CRASH research programme.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
#include <stdlib.h>
#include <string.h>
#include <cheriintrin.h>
#include "cheri_c_test.h"

void nullify(int **x) {
    for(int i = 0; i < 10; i++) {
        x[i] = NULL;
    }
}

BEGIN_TEST(libc_memcpy)
    int *a = malloc(sizeof(int) * 10);
    for(int i = 0; i < 10; i++) 
        a[i] = 3 * i;
    int **x = malloc(sizeof(int *) * 10);
    for(int i = 0; i < 10; i++) 
        x[i] = a + i;
    // preliminary correctness check
    for(int i = 0; i < 10; i++) 
        assert(*x[i] == i * 3);
    int **y = malloc(sizeof(int *) * 10);
    // TEST 1: aligned start and end
    // EXPECT: all copied capabilites should have their tag preserved
    memcpy(y + 1, x + 3, sizeof(int *) * 5);
    for(int i = 0; i < 10; i++) {
        // check original; this will be done only once.
        assert(cheri_tag_get(x[i]));
        assert(*x[i] == i * 3);
        // check copy
        if(i >= 1 && i < 6) {
            assert(cheri_tag_get(y[i]));
            assert(y[i] == x[i + 2]);
            assert(*y[i] == (i + 2) * 3);
        }
    }
    nullify(y);
    // TEST 2: misaligned start, aligned end
    // EXPECT: when memcpy eventually reaches an aligned position,
    //         provided suffient space tags should be preserved
    memcpy((char *)y + 16,(char *)x + 16, sizeof(int *) * 5 + 16);
    for(int i = 1; i < 6; i++) {
        assert(cheri_tag_get(y[i]));
        assert(y[i] == x[i]);
        assert(*y[i] == (i * 3));
    }
    nullify(y);
    // TEST 3: aligned start, misalgned end
    // EXPECT: all capabilities will be copied, whereas the end bit will be
    //         incomplete
    memcpy(y + 1, x + 3, sizeof(int *) * 5 + 16);
    for(int i = 1; i < 6; i++) {
        assert(cheri_tag_get(y[i]));
        assert(y[i] == x[i + 2]);
        assert(*y[i] == ((i + 2) * 3));
    }
    nullify(y);
    // TEST 4: misaligned start and end
    // EXPECT: remaining aligned parts will preserve the tag.
    memcpy((char *) y + 12, (char *) x + 12, sizeof(int *) * 6);
    for(int i = 1;  i < 6; i++) {
        assert(cheri_tag_get(y[i]));
        assert(y[i] == x[i]);
        assert(*y[i] == (i * 3));
    }
    // TEST 5: misaligned src & dst
    // EXPECT: if either one of src or dest is misaligned 
    //         then we copy invalidated tags.
    //         They should still have the same values.
    nullify(y);
    // note: it is impossible to load capabilities that are misaligned.
    //       So we use Z as the intermediate buffer and copy back to y aligned.
    int **z = malloc(sizeof(int *) * 11);
    memcpy((char *)z + 1, x, sizeof(int *) * 10);
    memcpy(y, (char *)z + 1, sizeof(int *) * 10);
    for(int i = 0; i < 10; i++) {
        assert(cheri_tag_get(x[i]));
        assert(!cheri_tag_get(y[i]));
        assert(x[i] == y[i]);
    }
    free(z);
    free(y);
    free(x);
    free(a);
END_TEST

