//===--------------------- test_target_get_max_teams.c ----------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test uses the omp_get_max_teams routine to check what the max teams 
// capacity is for this device. It should return the total amount of teams
// that could be allocated to a specific teams region
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
	int MAX_TEAMS = omp_get_max_teams();

	#pragma omp target teams map(tofrom: num_teams, MAX_TEAMS)
	{
		if (omp_get_team_num() == 0) {
			num_teams = omp_get_num_teams();
		}
	}               

	printf("%d %d %d\n", num_teams, MAX_TEAMS, OMPVV_NUM_TEAMS_DEVICE);

	OMPVV_ERROR_IF(num_teams != MAX_TEAMS, "Number of teams reported was not the max amount of teams");
	OMPVV_TEST_AND_SET(errors, num_teams != MAX_TEAMS);

	OMPVV_REPORT_AND_RETURN(errors);
}
