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

int test_case(){
	int errors = 0;
	int sum = 0;
	int test_lst[N];
	for (int i = 0; i<N; i++){
		test_lst[i] = 1;
	}
	#pragma omp target map(iterator(it = 0:N), tofrom: test_lst[it])
	{
		for(int i = 0; i < N; i++){
			test_lst[i] = 2;
		}
	}
	for (int i = 0; i < N; i++){
		sum += test_lst[i];
	}
	OMPVV_TEST_AND_SET(errors, (sum != 2*N));
	return (errors);
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors,test_case() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
