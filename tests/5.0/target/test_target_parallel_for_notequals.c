//===--- test_target_parallel_for_notequals.c -------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the parallel for directive with relational-op "!=".
// The test performs simple operations on an int array in device env 
// which are then checked for correctness. 
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_target_parallel_for_notequals() {
  OMPVV_INFOMSG("test_parallel_for_notequals_device");
  int errors = 0;
  int x[N];
  int y[N];
  int z[N];

  for (int i = 0; i < N; i++) {
    x[i] = 1;
    y[i] = i + 1;
    z[i] = 2*(i + 1);
  }

#pragma omp target parallel for num_threads(OMPVV_NUM_THREADS_HOST) map(to: y, z) map(tofrom: x)
  for (int i = 0; i != N; i++) {
    x[i] += y[i]*z[i];
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != 1 + (y[i]*z[i]));
  }

  return errors;
}


int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_parallel_for_notequals());

  OMPVV_REPORT_AND_RETURN(errors);
}
