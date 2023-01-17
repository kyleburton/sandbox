#include <gsl/gsl_cdf.h>
#include <stdio.h>
#include <sys/cdefs.h>

int __attribute__((warn_unused_result)) myfunc(int ii);
int myfunc(int ii) { return ++ii; }

/*
  int main(__attribute__((unused)) int argc, __attribute__((unused)) char
  **argv)
 */
int main(int argc, char **argv) {
  int res;
  double bottom_tail = gsl_cdf_gaussian_P(-1.96, 1);
  (void)argc;
  (void)argv;
  printf("Area between [-1.96, 1.96]; %g\n", 1 - 2 * bottom_tail);

  // trying to trigger unused-result
  res = myfunc(3);
  printf("myfunc(3)=%d\n", res);

  return 0;
}
