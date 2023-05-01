//===--------------------- test_target_set_num_teams.c ----------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// 
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
	int not_shared = 5;
	int num_teams = 0; 

	#pragma omp target teams map(tofrom:num_teams, errors) 
	{
		#pragma omp parallel
		{
			if (omp_get_team_num() == 0) {
				num_teams = omp_get_num_teams();
			}
		}
	}

	//Set the number of teams to be one less what the default is
	//omp_set_num_teams(num_teams - 1);
		

	#pragma omp target teams map(tofrom: num_teams, errors)
	{
		#pragma omp parallel
		{
			if (omp_get_team_num() == 0) {
				OMPVV_ERROR_IF(omp_get_num_teams() == num_teams, "Team count did not change when the teams limit was set");
				OMPVV_TEST_AND_SET(errors, omp_get_num_teams() == num_teams);
			}
		}
	}

	#pragma omp target teams num_teams(OMPVV_NUM_TEAMS_DEVICE)
	{
		#pragma omp parallel
		{
			if (omp_get_team_num() == 0) {
				OMPVV_ERROR_IF(omp_get_num_teams() != OMPVV_NUM_TEAMS_DEVICE, "Team count was not properly overriden by the num_teams clause");
				OMPVV_TEST_AND_SET(errors, omp_get_num_teams() != OMPVV_NUM_TEAMS_DEVICE);
			}
		}
	}

	OMPVV_REPORT_AND_RETURN(errors);
}
