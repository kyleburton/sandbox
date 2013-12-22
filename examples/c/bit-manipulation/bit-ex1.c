#include "stdio.h"

void printBits(size_t const size, void const * const ptr) {
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i=size-1;i>=0;i--) {
        for (j=7;j>=0;j--) {
            byte = b[i] & (1<<j);
            byte >>= j;
            printf("%u", byte);
        }
    }
}

int main (int argc, char**argv) {
  int ii = 0;
  unsigned long long val;

  printf("sizeof: %d\n", sizeof(unsigned long long));
  for(ii =0; ii < 8*(sizeof(unsigned long long)); ++ii) {
    val = ~((unsigned long long)0) >> ii;
    printf("%02d: ", ii);
    printBits(sizeof(unsigned long long), &val);
    printf(" %llu\n", val);
  }

  for(ii =0; ii < 8*(sizeof(unsigned long long)); ++ii) {
    val = ((unsigned long long)1) << ii;
    printf("%02d: ", ii);
    printBits(sizeof(unsigned long long), &val);
    printf(" %llu\n", val);
  }
  return 0;
}
