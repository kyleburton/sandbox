#include <algorithm>
#include <iostream>
#include <string>

int main2() {
    auto i{ 0x01B99644 };
	// auto i{ 0xF };
    std::string x{ " DFaeillnor" };
    std::cout << "typeid(i)=" << typeid(i).name() << std::endl;
    std::cout << "typeid(x)=" << typeid(x).name() << std::endl;
    while (i--) {
        std::next_permutation(x.begin(), x.end());
        if (0 == (i % 10000)) {
            std::cout << '.';
		} else if (i < 0xFF) {
			std::cout << x << std::endl;
		}
    }
    return 0;
}