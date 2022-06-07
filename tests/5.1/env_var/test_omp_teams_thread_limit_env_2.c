#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int test_case(){
	int errors = 0;
	int env = 10;
	env = omp_get_teams_thread_limit();
	OMPVV_TEST_AND_SET(errors, env != 2);
	OMPVV_INFOMSG_IF(env == 0, "Environment variable not set");
	OMPVV_INFOMSG_IF(env == 10, "Test variable not overwritten");
	return errors;
}


int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_case() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
