//===--- test_target_assumed_array_size.c ------------------------------------===//
//
// OpenMP API Version 6.0
// Tests the target directive with the map clause on an assumed-size array.  
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 1024

int test_assumed_array_size(){
	int errors = 0;
	int array[N];
	int *pointer;

	for (int i = 0; i < N; i++){
		array[i] = 0;
	}
	
	pointer = &array[0];
 	#pragma omp target data map(tofrom: array[0:N])	
	#pragma omp target parallel for map(pointer[:])
	for (int i = 0; i < N; i++){
		*pointer[i] = i;
	}


	for (int i = 0; i < N; i++){
		OMPVV_TEST_AND_SET_VERBOSE(errors, array[i] != i);
	}

	return errors;
}

int main(){
	OMPVV_TEST_OFFLOADING;
	int errors = 0;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_assumed_array_size() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
