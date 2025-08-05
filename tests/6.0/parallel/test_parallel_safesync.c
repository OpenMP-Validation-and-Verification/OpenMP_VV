//--------------- test_directive_clause.c ------------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 901, line 9
// ***********
// DIRECTIVE:parallel
// CLAUSE:safesync
// ***********
// Code is adapted from the safesync clause example in the 6.0 examples
// document. This example uses a ticket lock implementation to which executes in
// the target region. Certain offloading implementations may be unable to
// properly synchronize the threads pertaining to different progress groups at
// the end of the parallel region. The result of this can be deadlock. The
// safesync clause is suppossed to ensure that such synchronization is possible.
// If no deadlock or inporper update of the arrays occurs, then this indicates
// the safesync clause was working correctly.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <omp.h>

#define N 100

int test_directive() {
  int errors = 0;
  int a[N], b[N];
  int count1 = 0, count2 = 0;

  for (int i = 0; i < N; i++)
    b[i] = i;
  // clang-format off
  #pragma omp target thread_limit(OMPVV_NUM_THREADS_DEVICE) map(to: count1, count2) map(a) map(to:b)
  #pragma omp parallel num_threads(OMPVV_NUM_THREADS_DEVICE) safesync(1)
  {
    int t, u;
    #pragma omp atomic capture
    t = ++count1;
    do {
        #pragma omp atomic read acquire
        u = count2;
    } while (u < t);

    for (int i = 0; i < N; i++) {
        a[i] += b[i];
    }
    #pragma omp atomic release
    count2++;
  }
  // clang-format on

  for (int i = 0; i < N; i++) {
    if (a[i] != OMPVV_NUM_THREADS_DEVICE * b[i]) {
      ++errors;
    }
  }

  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET(errors, test_directive() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
  return errors;
}
