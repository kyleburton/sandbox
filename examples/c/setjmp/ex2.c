#include <stdio.h>
#include <setjmp.h>

jmp_buf saved_location;

int main (int argc, char **argv) {
  int counter = 10;
  int jmpval = 0;
  jmpval = setjmp(saved_location);

  if (jmpval == 0) {
    printf("jmp_buf initialized successfully\n");
  }
  else {
    printf("jump achieved: counter=%d; jmpval=%d\n", counter, jmpval);
    if (counter == 0) {
      printf("counter reached zero, terminating.\n");
      return 0;
    }
    --counter;
    longjmp(saved_location, 1);
    return 0;
  }

  printf("about to longjmp\n");
  longjmp(saved_location, 137);

  printf("after longjmp, can't get here");
  return 0;
}

