#include <gsl/gsl_cdf.h>
#include <stdio.h>

// From:
// https://stackoverflow.com/questions/12198449/cross-platform-macro-for-silencing-unused-variables-warning/12199209#12199209
#define MON_Internal_UnusedStringify(macro_arg_string_literal) #macro_arg_string_literal
#define MONUnusedParameter(macro_arg_parameter) _Pragma(MON_Internal_UnusedStringify(unused(macro_arg_parameter)))

int main(int argc, char **argv) {
  MONUnusedParameter(argc)
  MONUnusedParameter(argv)
  double bottom_tail = gsl_cdf_gaussian_P(-1.96, 1);
  printf("Area between [-1.96, 1.96]; %g\n", 1 - 2 * bottom_tail);
  return 0;
}
