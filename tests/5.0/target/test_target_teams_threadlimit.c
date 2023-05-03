//===------test_target_teams_threadlimit.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// This is a basic test to demonstrate target teams + num_teams + thread_limit
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define NUM_TEAMS_TO_SET 32
#define NUM_THREADS_TO_SET 16

int main() {
  OMPVV_TEST_OFFLOADING;
  int numThreads[NUM_TEAMS_TO_SET];
  int errors = 0;
  // Inititalize
  for (int i = 0; i < NUM_TEAMS_TO_SET; i++) {
    numThreads[i] = 0;
  }
  // Execute on target
#pragma omp target teams num_teams(NUM_TEAMS_TO_SET) thread_limit(NUM_THREADS_TO_SET)
  {
    numThreads[omp_get_team_num()] = omp_get_num_threads();
  }
  // Validate
  for (int i = 0; i < NUM_TEAMS_TO_SET; i++) {
    printf("Number of threads in teams = %d \n", numThreads[i]);
    OMPVV_TEST_AND_SET_VERBOSE(errors, (numThreads[i] > NUM_THREADS_TO_SET));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
