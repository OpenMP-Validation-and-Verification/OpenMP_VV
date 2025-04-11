//--------------test_task_threadset.c-----------------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 901, line 30
// ***********
// DIRECTIVE:task
// CLAUSE:threadset
// ***********
// This test checks the functionality of the threadset clause with the task
// directive. It verifies that the task is executed by a free-agent thread when
// threadset(omp_pool) is used, and that the result of the Fibonacci calculation
// is correct when threadset(omp_team) is used.
//----------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>

// not too large
#define N 10

int count = 0;

int fib_seq(int n) {
  if (n < 2)
    return n;
  return fib_seq(n - 1) + fib_seq(n - 2);
}

int fib(int n, int *result) {
  int i, j;
  if (n < 2) {
    if (omp_is_free_agent()) {
#pragma omp atomic
      count++;
    }
    *result = n;
    return n;
  }

  #pragma omp task shared(i)
  fib(n - 1, &i);
  #pragma omp task shared(j)
  fib(n - 2, &j);
  #pragma omp taskwait

  *result = i + j;
  return i + j;
}

void task_work(int n, int use_pool) {
  count = 0;
  int result;
  #pragma omp parallel
  {
    #pragma omp task threadset(use_pool ? omp_pool : omp_team)
    fib(n, &result);
  }
  // Verify if the task was executed by a free-agent thread when threadset is
  // omp_pool
  if (use_pool) {
    OMPVV_TEST_VERBOSE(count > 0); // count should be greater than 0 if a
                                   // free-agent thread executed the task
  } else {
    OMPVV_TEST_VERBOSE(
        count ==
        0); // count should be 0 if no free-agent thread executed the task
    OMPVV_TEST_AND_SET(errors, result != fib_seq(n));
  }
}

int errors = 0;

int test_task_threadset() {
  int n = N;

  // Test task with threadset(omp_pool)
  task_work(n, 1);

  // Test task with default threadset (should be omp_team)
  task_work(n, 0);

  // Test task with num_threads clause
  omp_set_num_threads(4);
  task_work(n, 0);

  return errors;
}

int main() {
  OMPVV_TEST_AND_SET(errors, test_task_threadset() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
