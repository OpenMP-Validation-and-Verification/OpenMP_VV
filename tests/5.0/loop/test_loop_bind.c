//===--- test_loop_bind.c ---------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the loop directive with the order(concurrent) clause. The
// order(concurrent) clause is assumed to be present if it is not present, so
// this test covers the standalone loop directive as well. The test creates a
// parallel region with a loop construct nested within, and performs simple
// operations on an int array which are then checked for correctness. Note
// that the number of
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_loop_bind() {
  OMPVV_INFOMSG("test_loop_bind");
  int errors = 0;
  int x[N][N];
  int y[N];
  int z[N];
  int num_threads = -1;

  for (int i = 0; i < N; i++) {
    x[i] = 1;
    y[i] = i;
    z[i] = 2*i;
  }

#pragma omp teams distribute num_teams(OMPVV_NUM_TEAMS_DEVICE) thread_limit(OMPVV_NUM_THREADS_HOST)
  for (int i = 0; i < N; i++) {
#pragma omp parallel num_threads(OMPVV_NUM_THREADS_HOST)
    {
#pragma omp loop binding(teams)
      for (int j = 0; j < N; j++) {
        x[i][j] += y[i]*z[i];
      }
      if (omp_get_thread_num() == 0) {
        num_threads = omp_get_num_threads();
      }
    }
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != 1 + (y[i]*z[i]));
  }

  OMPVV_WARNING_IF(num_threads == 1, "Test ran with one thread, so parallelism of loop construct can't be guaranteed.");
  OMPVV_TEST_AND_SET_VERBOSE(errors, num_threads < 1);
  OMPVV_ERROR_IF(num_threads < 1, "omp_get_num_threads() returned an invalid number of threads.");

  return errors;
}


int main() {
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_loop_bind());

  OMPVV_REPORT_AND_RETURN(errors);
}
