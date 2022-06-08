//--------------- test_taskwait_nowait.c---------------------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// Tests the behavior of taskwait when nowait is specified.
// The behavior of this should be similar to that of using task construct.
// Test was adapted to use the depend clauses which the nowait clause should ignore
// It is important to note that all depenend tasks must be finished first.
//-----------------------------------------------------------------------//


#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>
#include <unistd.h>

#define N 1024

int test_task_nowait(){
	int errors = 0;
	int test_scaler = 1;
	int test_arr[N];
	int sum = 0;
	for (int i =0; i<N; i++){
		test_arr[i] = 1;
		sum += i;
	}
	#pragma omp parallel
	#pragma omp single
	{
		#pragma omp task depend(inout: test_scaler) shared(test_scaler)
		{
			usleep(10);
			test_scaler += 1;
		}
		#pragma omp taskwait nowait depend(inout: test_scaler) depend(out: test_arr)
		#pragma omp task depend(inout : test_arr) shared(test_arr)
		{
			for (int i=0; i<N; i++){
				test_arr[i] += 1;
			}
		}
	}
	int new_sum = 0;
	for (int i = 0; i < N; i++){
		new_sum += test_arr[i];
	}
	OMPVV_TEST_AND_SET(errors, test_scaler != 2);
	OMPVV_INFOMSG_IF(test_scaler == 1, "Scaler task region failed");
	OMPVV_TEST_AND_SET(errors, new_sum != 2048);
	OMPVV_INFOMSG_IF(sum == new_sum, "Array taskwait region failed");
	return errors;

}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_nowait() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}

