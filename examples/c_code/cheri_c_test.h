#include <assert.h>

void test_setup(void);

#define	DECLARE_TEST(name, desc) \
    static const char test_ ## name ## _desc[] = (desc);
#define	DECLARE_TEST_FAULT	DECLARE_TEST
#define BEGIN_TEST(name) \
	int main(void) {
#define END_TEST \
    return 0; }

#ifndef assert_eq
#define assert_eq(a, b) assert((long)(a) == (long)(b))
#endif
#ifndef assert_eq_cap
#define assert_eq_cap(a, b) assert((a) == (b))
#endif

enum perm {
    LOAD,
    STORE,
    LOAD_CAPABILITY,
    STORE_CAPABILITY,
    STORE_LOCAL_CAPABILITY,
    GLOBAL
};

int i__builtin_cheri_perms_get(void *x, int);

#define ASSERT_HAS_PERMISSION(x, perm) \
        assert(i__builtin_cheri_perms_get(x, perm))


