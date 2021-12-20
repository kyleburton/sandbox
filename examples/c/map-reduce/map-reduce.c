// gcc -Wall map-reduce.c  && ./a.out
#include <stdio.h>

void map_i ( int(*fp)(int), int * elements, int num_elements ) {
  int ii;
  for (ii = 0; ii < num_elements; ++ii) {
    elements[ii] = fp(elements[ii]);
  }
}

// void float map_f ( f_fp_f fp, float * elements, int num_elements );

// and so on
//

int reduce_i ( int(*fp)(int,int), int initial, int * elements, int num_elements ) {
  int ii;
  for (ii = 0; ii < num_elements; ++ii) {
    initial = fp(initial,elements[ii]);
  }
  return initial;
}

int add_1(int ii) {
  return ii+1;
}

int sum (int ii, int jj) {
  return ii + jj;
}


int main ( int argc, char** argv ) {
  int elements[] = {1,2,3,4};
  int num_elements = sizeof(elements) / sizeof(*elements);
  map_i(add_1,elements,num_elements);
  int result = reduce_i(sum,0,elements,num_elements);
  printf("result=%d\n",result);
  return 0;
}
