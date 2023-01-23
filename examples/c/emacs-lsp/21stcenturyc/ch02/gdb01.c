#include <stdio.h>

static void print_array(int *elements, int count) {
  for (int ii = 0; ii < count; ++ii) {
    printf("  elements[%d]=%d\n", ii, elements[ii]);
  }
}

int main(int __attribute__((unused)) argc,
         char __attribute__((unused)) * *argv) {
  // size_t count = 20;
  int x[20] = {0};
  x[0] = 3;
  print_array(x, 20);
  return 0;
}
