//--------------- test_parallel_severity.c
//------------------------------------//
// OpenMP API Version 6.0 August 2024
// Pg. 318, line 22
// ***********
// DIRECTIVE:parallel
// CLAUSE:severity
// ***********
// The severity clause for the parallel directive is being tested. If the clause
// executes correctly, than the user will see no errors. Otherwise, the
// user should notice an error during compilation. If the entire construct
// executes incorrectly, the function return value should be non-zero.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>

int test_severity() {
  int return_value = 1;
  // clang-format off
  #pragma omp parallel severity(fatal)
  {
    if (omp_get_thread_num() == 0) --return_value;
  }
  // clang-format on

  return return_value;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_severity() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
