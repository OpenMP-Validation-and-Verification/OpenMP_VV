//===--- test_target_assumed_array_size.c ------------------------------------===//
//
// OpenMP API Version 6.0
//
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 1024

#pragma omp begin declare target

void update_array(int* array){

	#pragma omp target parallel map(tofrom: array[:N])
	for (int i = 0; i < N; i++){
		array[i] = i * 2;
	}

}

#pragma omp end declare target

int test_assumed_array_size(){
	int errors = 0;
	int *array = (int *)malloc(N * sizeof(int));

	for (int i = 0; i < N; i++){
		array[i] = i;
	}
	
	update_array(array);
	for(int i = 0; i < N; i++){
		OMPVV_TEST_AND_SET(errors, array[i] != i * 2);
	}
	free(array);
	return errors;
}

int main(){
	OMPVV_TEST_OFFLOADING;
	int errors = 0;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_assumed_array_size() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
