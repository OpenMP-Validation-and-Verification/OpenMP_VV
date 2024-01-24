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

int main() {


	int errors = 0; 
	int num_teams = 0;
	omp_set_num_teams(4);
        	
	#pragma omp teams 
	{
		if (omp_get_team_num() == 0 ) {
			num_teams = omp_get_num_teams();
		}
	}               
	
	OMPVV_ERROR_IF(num_teams <= 4, "Number of teams detected was not the number set by omp_set_num_teams()");
	OMPVV_TEST_AND_SET(errors, num_teams != 4);

	#pragma omp teams num_teams(OMPVV_NUM_TEAMS_HOST)
	{
		if (omp_get_team_num() == 0 ) {
			num_teams = omp_get_num_teams();
		}
	}

	OMPVV_ERROR_IF(num_teams != OMPVV_NUM_TEAMS_HOST, "Number of teams was not properly overriden by the num_teams clause");	
	OMPVV_TEST_AND_SET(errors, num_teams != OMPVV_NUM_TEAMS_HOST);
	OMPVV_REPORT_AND_RETURN(errors);
}
