//--------------- test_target_is_accessible_with_usm.c---------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// This test checks that the omp_target_is_accessible device routine.
// In this test the output of the target_is_accessible call should return
// true because the requires unified shared memory is used.
//-----------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 100

#pragma omp requires unified_shared_memory

int check_device(){
	int errors = -57;
	int check_test = 0;
	const int buf_size = sizeof(int) * N;
	const int dev = omp_get_default_device();

	int *ptr = (int *) malloc(buf_size);

	check_test = omp_target_is_accessible(ptr, buf_size, dev);
	
	free(ptr);

	OMPVV_TEST_AND_SET_VERBOSE(errors, check_test == 0);

	OMPVV_ERROR_IF(check_test == -57, "Call to omp_target_is_accessible did not return true or false");
	OMPVV_INFOMSG_IF((check_test != 0 && check_test != -57), "The host memory is accessible from the default device.");

	return errors;
}

int main(){
	int errors = 0;

	OMPVV_TEST_OFFLOADING;

	OMPVV_TEST_AND_SET_VERBOSE(errors, check_device() != 0);

	OMPVV_REPORT_AND_RETURN(errors);
	
}
