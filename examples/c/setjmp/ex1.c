#include <stdio.h>
#include <setjmp.h>

jmp_buf saved_location;

int main (int argc, char **argv) {
  int jmpval = 0;
  jmpval = setjmp(saved_location);
    if (jmpval == 0) {
      printf("jmp_buf initialized successfully\n");
    }
    else {
      printf("jump achieved: jmpval=%d\n", jmpval);
      return 0;
    }

    printf("about to longjmp\n");
    longjmp(saved_location, 137);

    printf("after longjmp, can't get here");
    return 0;
}

