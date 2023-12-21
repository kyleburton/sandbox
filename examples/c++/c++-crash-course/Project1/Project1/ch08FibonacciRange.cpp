#include "ch08.h"

FibonacciRange::FibonacciRange(int max)
	: max{ max } {}
FibonacciIterator FibonacciRange::begin() const {
	return FibonacciIterator{};
}
int FibonacciRange::end() const {
	return max;
}
