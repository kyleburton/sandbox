#include <cstdio>

void print_poitner_address(void* addr) {
	/*
	unsigned long long b1 = (0xFFFF000000000000 & (unsigned long long)addr) >> 48;
	unsigned long long b2 = (0x0000FFFF00000000 & (unsigned long long)addr) >> 32;
	unsigned long long b3 = (0x00000000FFFF0000 & (unsigned long long)addr) >> 16;
	unsigned long long b4 = (0x000000000000FFFF & (unsigned long long)addr) >>  0;
	*/
	unsigned int b1 = 0xFFFF & (unsigned int)((unsigned long long)addr >> 48);
	unsigned int b2 = 0xFFFF & (unsigned int)((unsigned long long)addr >> 32);
	unsigned int b3 = 0xFFFF & (unsigned int)((unsigned long long)addr >> 16);
	unsigned int b4 = 0xFFFF & (unsigned int)((unsigned long long)addr >>  0);

	printf("addr{%04X %04X %04X %04X}", b1, b2, b3, b4);
}

int main() {
	int gettysburg{};
	printf("gettysburg: %d\n", gettysburg);
	int* gettysburg_address = &gettysburg;
	printf("&gettysburg: 0x%p\n", gettysburg_address);
	printf("print_poitner_address: ");
	print_poitner_address(gettysburg_address);
	printf("\n");
	return 0;
}