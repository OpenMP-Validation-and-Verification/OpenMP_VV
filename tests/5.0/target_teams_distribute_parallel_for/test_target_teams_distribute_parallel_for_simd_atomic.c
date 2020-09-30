//===--- test_target_teams_distribute_parallel_for_simd_atomic.c ------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Adapted from OpenMP examples acquire_release.2.c
// When the atomic read operation on thread 1 reads a non-zero value from y,
// this results in a release/acquire synchronization that in turn implies that
// the assignment to x on thread 0 happens before the read of x on thread 1.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_target_teams_distribute_parallel_for_simd_atomic() {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_simd_atomic");
  int errors = 0, x = 0;

#pragma omp target teams distribute parallel for simd map(tofrom: x) shared(x) num_teams(OMPVV_NUM_TEAMS_DEVICE) num_threads(OMPVV_NUM_THREADS_DEVICE)
  for (int i = 0; i < N; i++) {
#pragma omp atomic update
    x += 1;
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, x != N)

  return errors;
}


int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_simd_atomic());

  OMPVV_REPORT_AND_RETURN(errors);
}
