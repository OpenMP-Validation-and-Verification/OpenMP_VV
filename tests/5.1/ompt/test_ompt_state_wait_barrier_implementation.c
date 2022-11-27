//----------------------test_ompt_state_wait_barrier_teams---------------------
//
//OpenMP API Version 5.1 Oct 2022
//
//This test is designed to test the ompt_state_t_barrier_implementation enum.
//The test will attempt to access the barrier_implementatoin enum and check
//to see if its value matches the value that is expected.
//
//-----------------------------------------------------------------------------


#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <omp-tools.h>
#include "ompvv.h"
#include <unistd.h>

int test_case(){
	int error_check = 0;
	int errors = 0;
	int test_val = 0;
	int enum_num = 21;
	//test_val = ompt_state_wait_barrier_implementation;
	//OMPVV_TEST_AND_SET_VERBOSE(errors, test_val != 21);
	//OMPVV_INFOMSG_IF(test_val == 0, "local test variable was not overwritten");

	#pragma omp teams shared(enum_num, error_check)
	{
		int state_val = ompt_get_state();
		if (state_val != enum_num){
			errors += 1;
		}
		usleep(10);
	}
	OMPVV_TEST_AND_SET_VERBOSE(errors, error_check != 0);
	OMPVV_INFOMSG_IF(error_check != 0, "a thread state was not teams wait");
	return errors;
}

int main() {
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_case() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
