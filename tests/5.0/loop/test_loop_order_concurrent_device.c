//===--- test_loop_order_concurrent_device.c --------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the loop directive with the order(concurrent) clause, in
// the context of a target construct. The order(concurrent) clause is assumed
// to be present if it is not present, so this test covers the standalone
// loop directive as well. The test creates a parallel region with a loop
// construct nested within, and performs simple operations on an int array
// which are then checked for correctness. Additionally, since loop binds to
// a parallel region the test checks that the threads all wait before
// proceeding out of the loop region. The number of threads is checked in the
// parallel region but after the loop construct because runtime API calls are
// not permitted in loop directive regions.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_loop_order_concurrent_device() {
  OMPVV_INFOMSG("test_loop_order_concurrent_device");
  int errors = 0;
  int total_wait_errors = 0;
  int x[N];
  int y[N];
  int z[N];
  int num_threads = -1;

  for (int i = 0; i < N; i++) {
    x[i] = 1;
    y[i] = i + 1;
    z[i] = 2*(i + 1);
  }

#pragma omp target parallel num_threads(OMPVV_NUM_THREADS_HOST) map(tofrom: x[0:N], y[0:N], z[0:N], num_threads, total_wait_errors)
  {
    int wait_errors = 0;
#pragma omp loop order(concurrent)
    for (int i = 0; i < N; i++) {
      x[i] += y[i]*z[i];
    }
    for (int i = 0; i < N; i++) {
      if (x[i] == 1) {
        wait_errors++;
      }
    }
#pragma omp atomic update
    total_wait_errors += wait_errors;
    if (omp_get_thread_num() == 0) {
      num_threads = omp_get_num_threads();
    }
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != 1 + (y[i]*z[i]));
  }

  OMPVV_WARNING_IF(num_threads == 1, "Test ran with one thread, so parallelism of loop construct can't be guaranteed.");
  OMPVV_TEST_AND_SET_VERBOSE(errors, num_threads < 1);
  OMPVV_ERROR_IF(num_threads < 1, "omp_get_num_threads() returned an invalid number of threads.");
  OMPVV_ERROR_IF(total_wait_errors, "Threads in target parallel region did not wait for loop region to finish before proceeding.");

  return errors + total_wait_errors;
}


int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_loop_order_concurrent_device());

  OMPVV_REPORT_AND_RETURN(errors);
}
