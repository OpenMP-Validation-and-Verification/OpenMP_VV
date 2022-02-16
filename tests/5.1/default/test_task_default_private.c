//-----------test_task_default_private.c---------------------------
//
// OpenMP API Version 5.1 Aug 2021
//
// Test the behavior of the default clasue when the specified
// data-sharing attribute is private. This test focuses on the
// use of the task and target construct and should work for both
// host and target implementations. Private should allow any data
// initialized outside of the task region to have its own instance
// within the task region and retain any value changes made within
// the region.
//-----------------------------------------------------------------

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define N 1024


int test_task_default_private(){
	int errors = 0;
	int test_num = 1;
	int test_arr[N];
	int sum = 0;
	int old_sum = 0;
	for (int i =0; i<N; i++){
		test_arr[i] = i;
		old_sum += i;
	}

	#pragma target map(tofrom: test_num, test_arr, sum)
	{
		#pragma omp task private(test_num, test_arr, sum)
		test_num += 1;
		for (int i = 0; i<N; i++){
			test_arr[i] = 1;
			sum += test_arr[i];
		}	

	}

	int new_sum;
	for (int i = 0; i<N; i++){
		new_sum += test_arr[i];
	}

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_num != 2);
	OMPVV_INFOMSG_IF(test_num == 1, "Scalar was not private, changes made inside task region were not kept");
	OMPVV_TEST_AND_SET_VERBOSE(errors, sum != new_sum);
        
	OMPVV_INFOMSG_IF(new_sum == old_sum, "Array was not private, changes made inside task region were not kept");
	printf("Hi");
	return errors;

}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_default_private() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
