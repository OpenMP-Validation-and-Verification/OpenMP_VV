//===--- test_target_teams_distribute_num_threads.c--------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the num_threads clause on a target teams distribute directive to
// indicate a requested number of threads to execute the teams distribute region.
// The specifications indicate that the number of threads that are given can be any
// number that is equal to or less than the indicated value. We first run a
// target teams distribute region without the clause to see what the default
// number of threads is, and then we use a value that is less than that in the
// test of the num_threads clause. If the region is run with more threads than
// indicated, the test errors. If the region is run with less threads than
// indicated, the test issues a warning since it is known that the device can
// run with more threads than was actually given.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main() {
  OMPVV_TEST_OFFLOADING;
  int default_threads;
  int num_threads;
  int errors = 0;

#pragma omp target teams distribute map(from: default_threads)
  for (int x = 0; x < N; ++x) {
    if (omp_get_team_num() == 0) {
      default_threads = omp_get_thread_limit();
    }
  }

  OMPVV_WARNING_IF(default_threads == 1, "Test operated with one thread. Cannot test thread_limit clause.");
  OMPVV_TEST_AND_SET(errors, default_threads <= 0);

  if (default_threads > 0) {
#pragma omp target teams distribute thread_limit(default_threads / 2) map(from: default_threads)
    for (int x = 0; x < N; ++x) {
      if (omp_get_team_num() == 0) {
        num_threads = omp_get_thread_limit();
      }
    }

    OMPVV_TEST_AND_SET(errors, num_threads > default_threads / 2);
    OMPVV_WARNING_IF(num_threads < default_threads / 2, "Test limited to fewer threads than were indicated. Still spec-conformant.");

  }

  OMPVV_REPORT_AND_RETURN(errors);
}
