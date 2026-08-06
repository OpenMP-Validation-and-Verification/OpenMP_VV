//--------------- test_parallel_safesync.c------------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 901, line 9
// ***********
// DIRECTIVE:parallel
// CLAUSE:safesync
// ***********
// Code is adapted from the safesync clause example in the 6.0 examples
// document. This example uses a ticket lock implementation which executes in
// the target region. Only a single thread per progress-unit will execute this region.
// If no deadlock or improper update of the arrays
// occurs, then this indicates the safesync clause is working correctly.
//----------------------------------------------------------------------------//
#include "ompvv.h"
#include <math.h>
#include <omp.h>

#define N 100

int test_directive() {
  int errors = 0;
  int a[N], b[N];
  int count1 = 0, count2 = 1;
  int num_threads = 0;

  for (int i = 0; i < N; i++)
    b[i] = i, a[i] = 0;

  #pragma omp target \
    map(to : count1, count2) map(a) map(to : b) map(tofrom : num_threads)
  #pragma omp parallel num_threads(OMPVV_NUM_THREADS_DEVICE) \
    safesync(omp_curr_progress_width)
  {
    if (omp_get_thread_num() % omp_curr_progress_width == 0) {
      if (omp_get_thread_num() == 0) {
        num_threads = omp_get_num_threads();
      }
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
  }

  for (int i = 0; i < N; i++) {
    if (a[i] != ceil(num_threads / omp_curr_progress_width) * b[i]) {
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
