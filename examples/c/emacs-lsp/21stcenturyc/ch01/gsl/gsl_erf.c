#include <gsl/gsl_cdf.h>
#include <stdio.h>

int main(int argc, char **argv) {
  double bottom_tail = gsl_cdf_gaussian_P(-1.96, 1);
  printf("Area between [-1.96, 1.96]; %g\n", 1 - 2 * bottom_tail);
  return 0;
}
