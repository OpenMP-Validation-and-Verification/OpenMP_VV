//===--- test_target_teams_distribute_thread_limit.c--------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the thread_limit clause on a target teams distribute directive to
// indicate a requested number of threads to execute the teams distribute region.
// The specifications indicate that the number of threads that are given can be any
// number that is equal to or less than the indicated value. We first run a
// target teams distribute region without the clause to see what the default
// number of maximum threads is, and then we use a value that is less than that in the
// test of the thread_limit clause. If the region is run with more threads than
// indicated, the test fails. If the region is run with less threads than
// indicated, the test issues a warning since it is known that the device can
// run with more threads than was actually given.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_target_teams_distribute_thread_limit(){
  int default_thread_limit;
  int default_threads[N];
  int num_thread_limit;
  int num_threads[N];
  int errors = 0;

#pragma omp target teams distribute map(from: default_thread_limit, default_threads)
  // default teams with no thread limit
  for (int x = 0; x < N; ++x) {
    default_thread_limit = omp_get_teams_thread_limit();
    if (omp_get_team_num() == 0) {
      #pragma omp parallel for
      for(int i = 0; i < N; i++){ default_threads[i] = omp_get_num_threads(); }
    }
  }

  OMPVV_WARNING_IF(default_thread_limit == 1, "Test operated with maximum of one thread. Cannot test thread_limit clause.");
  OMPVV_TEST_AND_SET(errors, default_thread_limit <= 0);
  for(int i = 0; i < N; i++){
    OMPVV_TEST_AND_SET_VERBOSE(errors, default_threads[i] <= 0);
  }

  // teams with thread limit
  if (default_thread_limit > 0) {
  #pragma omp target teams distribute thread_limit(default_thread_limit / 2) map(from: num_thread_limit, num_threads)
    for (int x = 0; x < N; ++x) {
      num_thread_limit = omp_get_thread_limit();
      if (omp_get_team_num() == 0) {
        #pragma omp parallel for
        for(int i = 0; i < N; i++){ num_threads[i] = omp_get_num_threads(); }
      }
    }
    OMPVV_TEST_AND_SET(errors, num_thread_limit > default_thread_limit / 2);
    OMPVV_WARNING_IF(num_thread_limit < default_thread_limit / 2, "Test was provided fewer threads than the thread_limit clause indicated. Still spec-conformant.");
    for(int i = 0; i < N; i++){ OMPVV_TEST_AND_SET_VERBOSE(errors, num_threads[i] > default_threads[i] / 2); }
  }
  return errors;

}

int main(){
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_thread_limit() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
