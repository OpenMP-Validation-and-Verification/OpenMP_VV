//--------------- test_parallel_message.c ------------------------------------//
// OpenMP API Version 6.0 August 2024
// Pg. 318, line 22
// ***********
// DIRECTIVE:parallel
// CLAUSE:message
// ***********
// The message clause for the parallel directive is being tested. If the clause
// executes correctly, than the user will see the given message. Otherwise, the
// message will be different, and the user will notice the difference. If the entire construct executes incorrectly, the function return value should be non-zero.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>

int test_message() {
  int return_value = 1;
  #pragma omp parallel message("Message clause was executed.")
  {
    if (omp_get_thread_num() == 0) --return_value;
  }

  return return_value;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET(errors, test_message() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
