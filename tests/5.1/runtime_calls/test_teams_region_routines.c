//===------------ test-teams-region-routines.c ----------------------------===//
//
// OpenMP API Version 5.1 Aug 2021
//
// Section 3.4.3 from
// https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-5-1.pdf
//
// Test for teams-region-routines added to 5.4. This routine sets number of
// teams and threads to be used by default
// in teams regions that do not specify them in the clause: 
// omp_set_num_teams           and   omp_get_max_teams 
// omp_set_teams_thread_limit  and   omp_get_teams_thread_limit
// based on the 5.0/teams/test_teams.c test
//
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"


int test_teams_region_routines()
{
  int num_teams[OMPVV_NUM_TEAMS_DEVICE];
  int num_threads[OMPVV_NUM_THREADS_DEVICE];
  int errors[2] = {0,0};
  int is_offloading;


  for (int x = 0; x < OMPVV_NUM_TEAMS_DEVICE; ++x) {
    num_teams[x] = -99;
  }

  for (int x = 0; x < OMPVV_NUM_THREADS_DEVICE; ++x) {
    num_threads[x] = -99;
  }

  //sets runtime test
  omp_set_num_teams(OMPVV_NUM_TEAMS_DEVICE);
  omp_set_teams_thread_limit(OMPVV_NUM_THREADS_DEVICE);

  //team error if the max teams doesn't get the set value
  OMPVV_TEST_AND_SET_VERBOSE(errors[0], omp_get_max_teams() != OMPVV_NUM_TEAMS_DEVICE);

  //thread error if the max teams doesn't get the set value
  OMPVV_TEST_AND_SET_VERBOSE(errors[1], omp_get_teams_thread_limit() != OMPVV_NUM_THREADS_DEVICE);

  // The test will also check if the correct team and thread values are passed
#pragma omp teams 
  {
    num_teams[omp_get_team_num()] = omp_get_num_teams();
#pragma omp parallel master
    num_threads[omp_get_team_num()]= omp_get_num_threads();
  }


  OMPVV_ERROR_IF(num_teams[0] != OMPVV_NUM_TEAMS_DEVICE, "Test ran with invalid number of teams.");

  OMPVV_ERROR_IF(num_threads[0] != OMPVV_NUM_THREADS_DEVICE, "Test returned an invalid number of threads.");


  for (int x = 1; x < num_teams[0]; ++x) {
    if (num_teams[x] != num_teams[x - 1]) {
      errors[0]++;
    }
    if (num_threads[x] != num_threads[x - 1]) {
      errors[1]++;
    }
  }
  
  OMPVV_INFOMSG_IF(!(errors[0] + errors[1]), "Test passed with %d teams and %d threads per team.", num_teams[0], num_threads[0]);

  return errors[0] + errors[1];
}


int main() {

  int errors = 0;

  OMPVV_TEST_OFFLOADING;

  OMPVV_TEST_AND_SET_VERBOSE(errors,  test_teams_region_routines()!= 0);

  OMPVV_REPORT_AND_RETURN(errors);
}


