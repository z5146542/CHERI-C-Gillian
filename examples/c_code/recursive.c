unsigned int fibonacci(unsigned int n) {
    if(n == 0)
        return 1;
    else if (n == 1)
        return 1;
    else 
        return fibonacci(n - 1) + fibonacci(n - 2);
}

int main() {
    unsigned int a0, a1, a2, a3, a4, a5, a6, a7, a8, a9;
    a0 = fibonacci(0);
    a1 = fibonacci(1);
    a2 = fibonacci(2);
    a3 = fibonacci(3);
    a4 = fibonacci(4);
    a5 = fibonacci(5);
    a6 = fibonacci(6);
    a7 = fibonacci(7);
    a8 = fibonacci(8);
    a9 = fibonacci(9);
    return 0;
}
