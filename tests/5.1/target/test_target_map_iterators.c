//-------------------------test_map_iterator.c-----------------------------
//
//OpenMP API Version 5.1 Jan 2023
//
// This test is designed to test the iterator map-type-modifier for the map
// clause. The test should create a list of size N and then pass to the 
// target region the length of the array 1:N and modify the values.
// The test then checks to see if the appropriate range was modified.
//
//-------------------------------------------------------------------------

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main(){
	
	OMPVV_TEST_OFFLOADING;

	int errors = 0;
	int test_lst[N];
	for (int i; i < N; i++){
		test_lst[i] = 1;
	}
	#pragma omp target map(iterator(it=1:N),tofrom: test_lst[it])
	{
		for (int i = 0; i < N-1; i++){
			test_lst[i] = 2;
		}

	}
	OMPVV_WARNING_IF(test_lst[0] != 1, "list 0 was overwritten");
	OMPVV_WARNING_IF(test_lst[1] != 2, "list 1 was not changed");
	OMPVV_TEST_AND_SET(errors, (test_lst[1] != 2));
	OMPVV_REPORTA_AND_RETURN(errors);
	return errors;

}
