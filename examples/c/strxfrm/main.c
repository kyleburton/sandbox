#include <stdio.h>
#include <string.h>

int main (int argc, char ** argv ) {
  char buff[8192];
  size_t s;
  buff[0] = '\0';
  for (int ii = 1; ii < argc; ++ii ) {
    s = strxfrm(buff, argv[ii], strlen(argv[ii]));
    fprintf(stderr, "arg[%d]: strxform(%s) => %zu / %s\n", ii, argv[ii], s, buff);
  }

  return 0;
}
