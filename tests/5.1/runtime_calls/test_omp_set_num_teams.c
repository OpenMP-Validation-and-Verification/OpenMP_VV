//===-- test_omp_set_num_teams.c --------------------------------------------===//
//
// OpenMP API Version 5.1 Aug 2021
//
// Section 3.4.3 from
// https://www.openmp.org/wp-content/uploads/OpenMP-API-Specification-5-1.pdf
//
// Test for omp_set_num_teams. This routine sets number of threads to be used
// in teams regions that do not specify a num_teams clause
// based on the 5.0/teams/test_teams.c test
//
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"


int test_omp_set_num_teams()
{
  int num_teams[OMPVV_NUM_TEAMS_DEVICE];
  int errors = 0;
  

  for (int x = 0; x < OMPVV_NUM_TEAMS_DEVICE; ++x) {
    num_teams[x] = -99;
  }

  omp_set_num_teams(OMPVV_NUM_TEAMS_DEVICE);
  #pragma omp teams
  {
    num_teams[omp_get_team_num()] = omp_get_num_teams();
  }

  if (num_teams[0] == 1) {
    OMPVV_WARNING("Test operated with 1 team. num_teams requested were %d.", OMPVV_NUM_TEAMS_DEVICE);
  } else if (num_teams[0] < 1) {
    OMPVV_ERROR("omp_get_num_teams() reported a value less than one.");
  }

  for (int x = 1; x < num_teams[0]; ++x) {
    if (num_teams[x] != num_teams[x - 1]) {
      errors++;
    }
  }

  OMPVV_INFOMSG_IF(!(errors), "Test passed with %d teams.", num_teams[0]);

  OMPVV_REPORT_AND_RETURN(errors);
}


int main() {

  //OMPVV_TEST_OFFLOADING;

  int errors = 0;
  //OMPVV_TEST_AND_SET_VERBOSE(errors, test_omp_set_num_teams() != 0);
  test_omp_set_num_teams();

  //OMPVV_REPORT_AND_RETURN(errors);
}

