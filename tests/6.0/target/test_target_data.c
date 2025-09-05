//--------------- test_target_data.c -----------------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 902, line 4
// ***********
// DIRECTIVE:target
// CLAUSE:data
// ***********
// DESCRIPTION
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>
int work_function(int var){
  return ++var;
}
int test_target_data() {
  int errors = 0;
  int value = 0;
  int a = 0;

  // clang-format off
  #pragma omp target_data map(tofrom: a)
  {
    #pragma omp target map(tofrom: a) nowait
    work_function(a)
  }
  // clang-format on

  OMPVV_ERROR_IF(a != 1, "Expected %i, received %i", 1, a);
  OMPVV_TEST_AND_SET(errors, value != 1);
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_directive() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
