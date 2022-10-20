int compare_lt(int a1, int a2) {
    return a1 < a2;
}

int compare_gt(int a1, int a2) {
    return a1 > a2;
}

int compare(int a1, int a2, void *cmp(int, int)) {
    return cmp(a1, a2);
}

int main() {
    int a1 = 3;
    int a2 = 5;
    int x = compare(a1, a2, compare_lt);
    int y = compare(a1, a2, compare_gt);
    return 0;
}
