//-------------test_scope_nowait_construct.c-------------//
//
//OpenMP API Version 5.1 Aug 2021
//
//Tests the behavior of the scope construct with nowait
//specified. This test should remove the implied barrier
//creatd by the scope construct when nowait is specified.
//-------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>
#include <unistd.h>

#define N 1024

int test_scope_nowait(){
	int errors = 0;
	int test_arr[N];
	for (int i=0; i<N, i++){
		test_arr[i] = 1;
		sum += i;
	}
	#pragma omp parallel shared(test_int)
	{
		#pragma omp scope nowait
		{
			for (int i=0; i<N; i++){
				test_arr[i] += 1;
			}
		}
	}
	int sum = 0;
	for (int i = 0; i < N; i++){
		sum += test_arr[i];
	}
	OMPVV_TEST_AND_SET_VERBOSE(errors, sum != 2048);
        OMPVV_INFOMSG_IF(test_int == 1024, "scope region was not accessed");
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_scope_nowait() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
