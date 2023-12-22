#include <cstdio>
#include <cmath>
#include "ch08.h"
#include <cstdint>
#include <vector>

struct PrimeIterator {
    PrimeIterator()
        : current{ 1 }
        , primes{ 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733 , 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821 , 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977, 983, 991, 997 }
	, op_plus_plus ( &PrimeIterator::operator_plus_plus_1 ){
	}

    bool operator!=(uint64_t xx) {
		 return xx >= current;
    }

    PrimeIterator& operator++() {
        return ((*this).*op_plus_plus)();
    }

    PrimeIterator& operator_plus_plus_1() {
        // move forward to the next prime
        do {
            ++current;
        } while (!is_prime(current));

        if (current > 2 && (current & 0x1) == 1) {
            op_plus_plus = &PrimeIterator::operator_plus_plus_odds_only;
        }

        return *this;
    }

    PrimeIterator& operator_plus_plus_odds_only() {
        // move forward to the next prime
        do {
			current += 2;
        } while (!is_prime_odds_only(current));

        return *this;
    }

    bool is_prime(uint64_t nn) {
        if (nn == 2) { return true; }
        if ((nn & 0x1) == 0) {
            return false;
        }
        return is_prime_odds_only(nn);
	}

    bool is_prime_odds_only(uint64_t nn) {
		uint64_t max = static_cast<uint64_t>(std::sqrt(nn));
        static uint64_t num_primes = primes.size();
		for (uint64_t ii = 0; ii < num_primes; ++ii) {
            if (nn % primes[ii] == 0) {
                return false;
			}
		}

		for (uint64_t ii = primes[num_primes - 1]; ii <= max; ii += 2) {
			if (nn % ii == 0) {
				return false;
			}
		}
        primes.push_back(nn);
		return true;
    }

    uint64_t operator*() const {
        return current;
    }

private:
	std::vector<uint64_t> primes;
    uint64_t current{ 1 };
    PrimeIterator& (PrimeIterator::* op_plus_plus)();
};

struct PrimeRange {
    explicit PrimeRange(uint64_t max) : max{ max } { }

    PrimeIterator begin() const {
        return PrimeIterator{};
    }

    uint64_t end() const {
        return max;
    }

private:
    uint64_t max;
};

int ch08_main() {
    for (const auto i : FibonacciRange{ 5000 }) {
        printf("fib| %d\n", i);
    }

    if (true) {
		uint64_t num_primes = 0;
		// uint64_t max = 0x0000000100000000;
		uint64_t max = 0x0000000001000000;
		for (const auto ii : PrimeRange{ max }) {
			// printf("prime| %lld\n", ii);
			num_primes++;
		}
		printf("num_primes=%lld\n", num_primes);
    }
    else {
        for (const auto ii : PrimeRange{ 1000 }) {
             printf("prime| %lld\n", ii);
        }
    }
    return 0;
}