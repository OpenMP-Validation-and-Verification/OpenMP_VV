//--------------- test_directive_clause.c ------------------------------------//
// OpenMP API Version //Number Month Year
// Pg. X, line X
// ***********
// DIRECTIVE:
// CLAUSE:
// ***********
// DESCRIPTION
//----------------------------------------------------------------------------//
#include "ompvv.h"
// #include <omp.h>

int test_directive() {
  int errors = 0;
  int value = 0;

  // clang-format off
  #pragma omp target_data map(a)
  {
    #pragma omp target map(a) nowait
    do_stuff_with_a(a);
  }
  // clang-format on

  OMPVV_ERROR_IF(value != 0, "Expected %i, received %i", 0, value);
  OMPVV_TEST_AND_SET(errors, value != 0);
  return errors;
}

int main() {
  int errors = 0;
  // requires omp.h
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_directive() != 0);
  // omp_display_env(1);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
