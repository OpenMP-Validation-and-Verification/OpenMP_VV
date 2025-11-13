//--------------- test_target_is_accessible.c---------------------//
//
// OpenMP API Version 5.1 Aug 2020
//
// This test checks the omp_target_is_accessible device routine.
// In this test if the output of the target_is_accessible returns
// true then the pointer on the host should be a valid pointer in the 
// device environment.
//-----------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int check_device(){
	int errors = 0;
	int check_test = 2;
	const int N = 100;
	const int buf_size = sizeof(int) * N;
	const int dev = omp_get_default_device();

	int *ptr = (int *) malloc(buf_size);

	check_test = omp_target_is_accessible(ptr, buf_size, dev);

	if(check_test)
	{
		#pragma omp target firstprivate(ptr)
			for (int i=0; i<N; i++)
				ptr[i] = 5*i;
		for (int i = 0; i < N; i++)
			OMPVV_TEST_AND_SET(errors, ptr[i] != 5*i);
	}else{
		OMPVV_WARNING_IF(check_test == 0, "omp_target_is_accessible returned false. This test will be skipped.\n");
		return OMPVV_SKIPPED_EXIT_CODE;
	}

	free(ptr);
	
	OMPVV_INFOMSG_IF(check_test != 0, "omp_target_is_accessible returned true");
	
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, check_device() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
