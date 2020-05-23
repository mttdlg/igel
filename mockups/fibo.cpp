/*

//
// Ideally, the following 'Fibonacci sequence' example ....
// 

proc fibo {a: Int = 0 ; b: Int = 1} {
    var c: Int = [add a b]

    print [hex a]
    print [hex b]

    while [le c 65535] {
        print [bin c 16] " -> " [hex c 4]
        set a b
        set b c
        set c [add a b]
    }

    print "---> "
    print [hex c]
    print [bin c]
}

fibo 0 1

//
// ...would produce something similar to the following
// (mock-up) C++ code/ when IGEL is set to a C++ back-end:
//

*/


#include <iostream>

void fibo(int a = 0, int b = 1) {
    int c = a + b;

    std::cout << IGEL::hex(a) << std::endl;
    std::cout << IGEL::hex(b) << std::endl;

    while (c < 65535) {
        std::cout << IGEL::bin(c, 16) << " -> " << IGEL::hex(c, 4) << std::endl;
        a = b;
        b = c;
        c = a + b;
    }
    std::cout << "---> " << std::endl;
    std::cout << IGEL::hex(c) << std::endl;
    std::cout << IGEL::bin(c) << std::endl;
}

int main(int argc, char* argv[]) {
    fibo(0, 1);
};
