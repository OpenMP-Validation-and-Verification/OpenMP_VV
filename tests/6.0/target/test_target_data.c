//--------------- test_target_data.c -----------------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 902, line 4
// ***********
// DIRECTIVE:target
// CLAUSE:data
// The target_data directive is being used in three scenaries of various task
// configurations for the same functional task. Because target_data is a
// task-generating composite construct, it will have expected effects that are
// being tested in each scenario. If the work_function executes correctly, then
// the value of a should expectedly change to 1.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>

int work_function(int var) { return var + 1; }
void check_function(int var, int *errors) {
  OMPVV_ERROR_IF(var != 1, "Expected %i, received %i", 1, var);
  OMPVV_TEST_AND_SET(*errors, var != 1);
  return;
}

int test_target_data() {
  int errors = 0;
  int a = 0;

  #pragma omp target_data map(tofrom : a)
  {
    #pragma omp target map(tofrom : a) nowait
    a = work_function(a);
  }
  check_function(a, &errors);
  a = 0;
  #pragma omp target_data map(tofrom : a) nogroup
  {
    #pragma omp target map(tofrom : a) nowait
    a = work_function(a);
  }
  check_function(a, &errors);
  a = 0;
  #pragma omp target_data map(tofrom : a) depend(inout : a)
  {
    #pragma omp target map(tofrom : a) nowait depend(out : a)
    a = work_function(a);
  }
  check_function(a, &errors);

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_target_data() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
