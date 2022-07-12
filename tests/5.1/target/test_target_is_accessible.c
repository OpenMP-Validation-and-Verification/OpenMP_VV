//--------------- test_target_is_accessible.c---------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// This test checks that the omp_target_is_accessible device routine.
// In this test the output of the target_is_accessible call should return
// true because the storage indicated by the first and second arguements
// is accessible by the targeted device. This test is closely adapdted
// from the 5.1 OpenMP example sheet.
//-----------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int check_device(){
	int errors = 0;
	int isSharedMemory = 0;
	int check_test = 2;
	const int N = 100;
	const int buf_size = sizeof(int) * N;
	const int dev = omp_get_default_device();
	
	/*Assumes taht one shared-memory systems, no copy is done*/
	#pragma omp target map(to: isSharedMemory)
		isSharedMemory = 1;

	int *ptr = (int *) omp_target_malloc(buf_size,dev);

	check_test = omp_target_is_accessible(ptr, buf_size, dev);
	
	if(check_test)
		{
		#pragma omp target firstprivate(ptr)
			for (int i=0; i<N; i++)
				ptr[i] = 5*i
		for (int i = 0; i < N; i++)
			OMPVV_TEST_AND_SET(errors, ptr[i] != 5*i);
		}

	free(ptr);
	OMPVV_TEST_AND_SET_VERBOSE(errors, check_test != isSharedMemory);
	OMPVV_INFOMSG_IF(check_test == 1, "Omp_target_is_accessible is 1");
	OMPVV_INFOMSG_IF(check_test == 0, "Omp_target_is_accessible is 0");
	OMPVV_ERROR_IF(check_test == 2, "omp_target_is_accessible did not return true or false");
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, check_device() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
