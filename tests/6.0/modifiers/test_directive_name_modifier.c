//--------------- test_directive_name_modifier.c -----------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 898, line 3
// CLAUSE:if, private
// All modifiers to clauses were extended to accept directive-name-modifiers.
// In this case, the private clause is being used with the parallel
// directive-name-modifier. If the threads correctly privatize thread_id,
// then the test will pass.
//----------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>

#define N OMPVV_NUM_THREADS_HOST

int test_directive_name_modifier() {
  int errors = 0;
  int thread_array[N] = {0};
  int thread_id = -1;

#pragma omp parallel for private(parallel : thread_id)
  {
    for (int i = 0; i < N; i++) {
      thread_id = omp_get_thread_num();
      thread_array[thread_id] = thread_id;
    }
  }

  for (int i = 0; i < N; ++i) {
    for (int j = i + 1; j < N; ++j) {
      if (thread_array[i] == thread_array[j])
        errors++;
    }
  }

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_directive_name_modifier() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
