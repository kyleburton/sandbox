#include "ch08.h"

bool FibonacciIterator::operator!=(int x) const {
	return x >= current;
}

FibonacciIterator& FibonacciIterator::operator++() {
	const auto tmp = current;
	current += last;
	last = tmp;
	return *this;
}

int FibonacciIterator::operator*() const {
	return current;
}

