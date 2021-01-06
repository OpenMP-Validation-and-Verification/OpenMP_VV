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

#define N 32

int test_loop_bind_teams() {
  OMPVV_INFOMSG("test_loop_bind_teams");
  int errors = 0;
  int x[N];
  int y[N];
  int z[N];
  int num_teams = -1;

  for (int i = 0; i < N; i++) {
    x[i] = 1;
    y[i] = i;
    z[i] = 2*i;
  }

#pragma omp teams num_teams(OMPVV_NUM_TEAMS_DEVICE) thread_limit(OMPVV_NUM_THREADS_HOST)
  {
#pragma omp loop bind(teams)
    for (int i = 0; i < N; i++) {
      x[i] += y[i]*z[i];
    }
    if (omp_get_team_num() == 0) {
      num_teams = omp_get_num_teams();
    }
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != 1 + (y[i]*z[i]));
  }

  OMPVV_WARNING_IF(num_teams == 1, "Test ran with one team, so parallelism of loop construct with bind(teams) can't be guaranteed.");
  OMPVV_TEST_AND_SET_VERBOSE(errors, num_teams < 1);
  OMPVV_ERROR_IF(num_teams < 1, "omp_get_num_teams() returned an invalid value.");

  return errors;
}

int test_loop_bind_parallel() {
  OMPVV_INFOMSG("test_loop_bind_parallel");
  int errors = 0;
  int x[N][N];
  int y[N];
  int z[N];
  int num_threads = -1;

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x[i][j] = 1;
    }
    y[i] = i;
    z[i] = 2*i;
  }

#pragma omp teams distribute num_teams(OMPVV_NUM_TEAMS_DEVICE) thread_limit(OMPVV_NUM_THREADS_HOST)
  for (int i = 0; i < N; i++) {
#pragma omp parallel num_threads(OMPVV_NUM_THREADS_HOST)
    {
#pragma omp loop bind(parallel)
      for (int j = 0; j < N; j++) {
        x[i][j] += y[i]*z[i];
      }
      if (omp_get_thread_num() == 0 && omp_get_team_num() == 0) {
        num_threads = omp_get_num_threads();
      }
    }
  }

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, x[i][j] != 1 + (y[i]*z[i]));
    }
  }

  OMPVV_WARNING_IF(num_threads == 1, "Test ran with one thread, so parallelism of loop construct can't be guaranteed.");
  OMPVV_TEST_AND_SET_VERBOSE(errors, num_threads < 1);
  OMPVV_ERROR_IF(num_threads < 1, "omp_get_num_threads() returned an invalid number of threads.");

  return errors;
}

int test_loop_bind_thread() {
  OMPVV_INFOMSG("test_loop_bind_thread");
  int errors = 0;
  int x[N][N];
  int y[N];
  int z[N];
  int num_threads = -1;

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x[i][j] = 1;
    }
    y[i] = i;
    z[i] = 2*i;
  }

#pragma omp teams distribute num_teams(OMPVV_NUM_TEAMS_DEVICE) thread_limit(OMPVV_NUM_THREADS_HOST)
  for (int i = 0; i < N; i++) {
#pragma omp parallel num_threads(OMPVV_NUM_THREADS_HOST)
    {
#pragma omp loop bind(thread)
      for (int j = 0; j < N; j++) {
        x[i][j] += y[i]*z[i];
      }
      if (omp_get_thread_num() == 0 && omp_get_team_num() == 0) {
        num_threads = omp_get_num_threads();
      }
    }
  }

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, x[i][j] != (1 + num_threads*(y[i]*z[i])));
    }
  }

  OMPVV_WARNING_IF(num_threads == 1, "Test ran with one thread, so parallelism of loop construct can't be guaranteed.");
  OMPVV_TEST_AND_SET_VERBOSE(errors, num_threads < 1);
  OMPVV_ERROR_IF(num_threads < 1, "omp_get_num_threads() returned an invalid number of threads.");

  return errors;
}

int main() {
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_loop_bind_teams());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_loop_bind_parallel());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_loop_bind_thread());

  OMPVV_REPORT_AND_RETURN(errors);
}
