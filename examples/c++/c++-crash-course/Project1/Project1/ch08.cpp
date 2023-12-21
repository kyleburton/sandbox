#include <cstdio>
#include <cmath>
#include "ch08.h"

struct PrimeIterator {
    bool operator!=(int xx) const {
        return xx >= current;
    }

    PrimeIterator& operator++() {
		// move forward to the next prime
        do {
            ++current;
        } while (!is_prime(current));
        return *this;
    }

    bool is_prime(int nn) {
        int max = static_cast<int>(std::sqrt(nn));
        for (int ii = 2; ii <= max; ++ii) {
            if (nn % ii == 0) {
                return false;
			}
		}
        return true;
    }

    int operator*() const {
        return current;
    }

private:
    int current{ 1 };
};

struct PrimeRange {
    explicit PrimeRange(int max) : max{ max } { }

    PrimeIterator begin() const {
        return PrimeIterator{};
    }

    int end() const {
        return max;
    }

private:
    int max;
};

int main() {
    for (const auto i : FibonacciRange{ 5000 }) {
        printf("fib| %d\n", i);
    }

    for (const auto ii : PrimeRange{ 5000 }) {
        printf("prime| %d\n", ii);
    }
}