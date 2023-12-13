//===--------------------- test_target_set_num_teams.c ----------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test uses the omp_set_num_teams utility to set the default number of
// teams allocated for team clauses. The test runs teams and checks to see if
// the number allocated is correct and reflective of the new default set. It
// then checks to see if an explicitly stated num_teams clause overrides
// the omp_set_num_teams properly.
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main() {

	OMPVV_TEST_OFFLOADING;

	int errors = 0; 
	int num_teams = 0;

#pragma omp target
{
	omp_set_num_teams(1);
}
	
	#pragma omp target teams map(tofrom: num_teams)
	{
		if (omp_get_team_num() == 0) {
			num_teams = omp_get_num_teams();
		}
	}               
	
	OMPVV_ERROR_IF(num_teams != 1, "Number of teams detected was not the amount set by omp_set_num_teams()");
	OMPVV_TEST_AND_SET(errors, num_teams != 1);

	#pragma omp target teams map(tofrom: num_teams) num_teams(OMPVV_NUM_TEAMS_DEVICE)
	{
		if (omp_get_team_num() == 0) {
			num_teams = omp_get_num_teams();
		}
	}

	OMPVV_ERROR_IF(num_teams != OMPVV_NUM_TEAMS_DEVICE, "Number of teams was not properly overriden by the num_teams clause");
	OMPVV_TEST_AND_SET(errors, num_teams != OMPVV_NUM_TEAMS_DEVICE);

	OMPVV_REPORT_AND_RETURN(errors);
}
