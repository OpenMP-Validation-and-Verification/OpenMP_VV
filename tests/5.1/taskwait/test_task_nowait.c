//--------------- test_taskwait_nowait.c---------------------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// Tests the behavior of taskwait when nowait is specified.
// The behavior of this should be similar to that of using task construct.
// It is important to note that all depenend tasks must be finished first.



#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_task_nowait(){
	int errors = 0;
	int test_scaler = 1;
	int test_arr[N];
	int sum = 0;
	for (int i =0; i<N; i++){
		test_arr[i] = i;
		sum += i;
	}
	#pragma omp task private(test_scaler)
	{
		test_scaler += 1;
	}
	#pragma omp task private(test_arr)
	{
		for (int i=0; i<N; i++){
			test_arr[i] = i;
		}
	}
	#pragma omp taskwait
	int new_sum;
	for (int i = 0; i < N; i++){
		new_sum += test_arr[i];
	}
	OMPVV_TEST_AND_SET(errors, test_scaler != 2);
	OMPVV_INFOMSG_IF(test_scaler == 1, "Scalar taskwait region was not ran");
	OMPVV_INFOMSG_IF(test_scaler > 2, "Scalar taskwait region was ran more than once");
	OMPVV_TEST_AND_SET(errors, sum == new_sum);
	OMPVV_INFOMSG_IF(sum == new_sum, "Array taskwait region was not ran");
	return errors;

}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_nowait() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}

