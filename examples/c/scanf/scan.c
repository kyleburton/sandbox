#include <stdio.h>
#include <stdlib.h>

int main (int argc, char** argv ) {
  int ii  = 0;
  int items_consumed = 0;
  int chars_consumed = 0;

  items_consumed = sscanf("1234", "%d%n", &ii,&chars_consumed);
  printf("after %%d scan chars_consumed=%d, items_consumed=%d, ii=%d\n",chars_consumed,items_consumed,ii);
  return 0;
}
