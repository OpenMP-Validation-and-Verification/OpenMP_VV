//--------------- test_omp_set_teams_thread_limit_routine.c ----------------//
//
// OpenMP API Version 5.1 Aug 2020
//
// The objective of this test is to check that the teams-thread-limit-var ICV
// is set properly. This test itself simply checks
// that the threadlimit is set to the appropriate variable value specified
// in the test name.//////////////
//--------------------------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

int test_set_teams_thread_limit() {
  int errors = 0;
  int limit_value = OMPVV_NUM_THREADS_DEVICE / OMPVV_NUM_TEAMS_DEVICE;
  int current_limit_value = omp_get_teams_thread_limit(); // default is 0

  if (limit_value == 1)
    limit_value = 2;

  omp_set_teams_thread_limit(limit_value);
  current_limit_value = omp_get_teams_thread_limit();

  OMPVV_ERROR_IF(current_limit_value != limit_value,
                 "teams-thread-limit-var ICV not set correctly");
  OMPVV_INFOMSG_IF(current_limit_value == 0,
                   "teams-thread-limit-var ICV is default value");
  OMPVV_TEST_AND_SET(errors, current_limit_value != limit_value);
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_set_teams_thread_limit() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
