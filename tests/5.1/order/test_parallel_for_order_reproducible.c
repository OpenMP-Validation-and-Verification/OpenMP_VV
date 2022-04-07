//===------------ test_target_parallel_for_order_reproducible.c -------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test checks that the order(reproducible) clause is properly handled.
// Leverages a shared variable to force a proper execution order to get proper
// results. Creates two arrays, one the correct method executed in order and
// the second executed from within the parallel for order pragma. Fails if
// the array values are not calculated in the proper order.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int main() {
	int errors = 0;
	int arr[N];
	int correct[N];
	int shared = 1;

	for (int i = 0; i < N; i++) {
		arr[i] = 0;
  	}

	for (int i = 0; i < N; i++) {
		correct[i] = shared;
		shared += i;
	}

	shared = 1;

	OMPVV_TEST_OFFLOADING;

	#pragma omp target map(tofrom:arr,errors,correct,shared)
	{
		#pragma parallel for thread_limit(OMPVV_NUM_THREADS_DEVICE) shared(arr, shared) order(concurrent)
		{
			for (int i = 0; i < N; i++) {
				arr[i] = shared;
				shared += i;	
			}
			
			for (int i = 0; i < N; i++) {
				OMPVV_TEST_AND_SET(errors, arr[i] != correct[i]);
			}

		}
	
	}

	OMPVV_REPORT_AND_RETURN(errors);
}
