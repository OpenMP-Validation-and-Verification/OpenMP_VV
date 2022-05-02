//===-----------------------test_num_threads_team_limit.c------------------===//
//This test aims to test the OMP_NUM_TEAMS and OMP_TEAMS_THREAD_LIMIT 
//environment variables. This is done by setting the number of teams and the 
//limit of number of threads in each team and then performing a simple 
//calculation to ensure it is set correctly.
//===---------------------------------------------------------------------===/

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main(){
	setenv("OMP_NUM_TEAMS", "2", 1);
	char * num_teams = getenv("OMP_NUM_TEAMS");
	printf("NUM TEAMS = %s", num_teams);
	unsetenv("OMP_NUM_TEAMS");
}
