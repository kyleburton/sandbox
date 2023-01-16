#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  char *home = getenv("HOME");
  if (NULL == home) {
    printf("Error: HOME not found!\n");
    return 1;
  }
  printf("HOME=%s\n", home);
  return 0;
}
