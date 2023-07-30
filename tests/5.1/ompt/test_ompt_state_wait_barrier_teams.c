//-----------------test_ompt_state_wait_barrier_teams-----------------------
//
// OpenMP API Version 5.1 Oct 2022
//
// This test is designed to test the ompt_state_t barrier teams member
// field. The test will access the ompt_state_wait_barrier_teams to ensure 
// it is being properly set.
// 
// ------------------------------------------------------------------------

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <omp-tools.h>
#include "ompvv.h"

int test_case(){
	int errors = 0;
	int test_val = 0;
  #pragma omp teams
  {
    if (omp_get_team_num() == 0)
	    test_val = ompt_state_wait_barrier_teams;
  }
  #pragma omp barrier
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_val != 22);
	OMPVV_INFOMSG_IF(test_val == 0, "local test variable was not overwritten");
	return errors;
}

int main() {
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_case() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
