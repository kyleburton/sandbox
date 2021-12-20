/* second.c */
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <unistd.h>

char* int_to_binary_ascii( int x ) {
  static char buff[8*sizeof(int) + 1];
  unsigned int ii, num_bits = (8 * sizeof(unsigned int));
  memset(buff,'\0',sizeof(buff));
  for ( ii = 0; ii < num_bits; ii++ ) {
    buff[num_bits - ii - 1] = ( x & (1 << ii ) ) ? '1' : '0';
  }
  return buff;
}

/* Use a terminal escape to clear the line...*/
void clear_line () {
  printf("%c[2K\r", 27);
}

int main (void) {
  long ii;
  char c;
  for ( ii=0;; ++ii ) {
    clear_line();
    printf("% 10ld: %s",ii, int_to_binary_ascii(ii));
    fflush(stdout);
    read(0,&c, sizeof(char));
    if ( 'q' == c ) {
      break;
    }
  }
  return 0;
}
