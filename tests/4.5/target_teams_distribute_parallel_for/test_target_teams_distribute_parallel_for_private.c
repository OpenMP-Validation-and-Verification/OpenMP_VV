//===---- test_target_teams_distribute_parallel_for_private.c - combined consutrct -===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test check for a private variable within a pragma omp target teams distribute
// parallel for. We use a private variable within a for loop and asign it every iteration
// hoping that we won't get into data races. We do this multiple times to improve
// test
// we assign a large number of threads and teams to try to increase parallelism and
// contention on the privatized variable
//
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>

#define SIZE_N 1024

int test_target_teams_distribute_parallel_for_private() {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_devices");

  int a[SIZE_N];
  int b[SIZE_N];
  int c[SIZE_N];
  int d[SIZE_N];
  int privatized = 10;
  int num_teams[SIZE_N];
  int num_threads[SIZE_N];
  int errors = 0;
  int i, j, dev;

  // array initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
    b[i] = i;
    c[i] = 2*i;
    d[i] = 0;
    num_teams[i] = -1;
    num_threads[i] = -1;
  }

  // check multiple sizes.
#pragma omp target data map(to: a[0:SIZE_N], b[0:SIZE_N], c[0:SIZE_N]) map(from: d[0:SIZE_N])
  {
#pragma omp target teams distribute parallel for private(privatized, i) num_threads(OMPVV_NUM_THREADS_DEVICE) num_teams(OMPVV_NUM_TEAMS_DEVICE)
    for (j = 0; j < SIZE_N; ++j) {
      num_teams[j] = omp_get_num_teams();
      num_threads[j] = omp_get_num_threads();

      privatized = 0;
      for (i = 0; i < a[j] + b[j]; ++i) {
        privatized++;
      }
      d[j] = c[j] * privatized;
    }
  }

  int warning_threads = 0;
  int warning_teams = 0;

  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, d[i] != (1 + i)*2*i);
    warning_teams += num_teams[i];
    warning_threads += num_threads[i];
  }

  OMPVV_WARNING_IF(warning_teams == SIZE_N, "There was a single team across the target region. Privatization cannot be tested at the teams level");
  OMPVV_WARNING_IF(warning_threads == SIZE_N, "All the parallel regions ran with a single thread. Privatization cannot be tested at the thread level");

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_private());

  OMPVV_REPORT_AND_RETURN(errors);
}
