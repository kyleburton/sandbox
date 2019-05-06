#include "stdio.h"

int main (int argc, char **argv ) {
  int counter = 0;
  printf("main: counter=%02d\n", ++counter);
  printf("main: counter=%02d\n", ++counter);
  printf("main: counter=%02d\n", ++counter);
  printf("main: counter=%02d\n", ++counter);
  printf("main: kthxbai!\n");
  return 0;
}
