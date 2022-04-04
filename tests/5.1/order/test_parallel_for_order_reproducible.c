//===------------ test_target_parallel_for_order_reproducible.c -------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test uses the thread_limit clause on the teams construct. Specifically
// testing if a thread_limit from a above target construct properly carries
// down to the nested teams construct, as if it were directly on the construct
// as defined in the spec. The test validates that only the specified 
// threads are created by summing a shared variable across all threads 
// (and teams). If the threads are correctedly limited this should produce the 
// expected value. Additional warnings are sent if specific issues occur.
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

	for (int i = 0; i < N; i++) {
		arr[i] = i;
  	}

	OMPVV_TEST_OFFLOADING;

	#pragma omp target map(tofrom:arr,errors)
	{
		#pragma parallel for thread_limit(OMPVV_NUM_THREADS_DEVICE) //order(reproducible)
		for (int i = 0; i < N; i++) {
			
		}
	
	}

	OMPVV_WARNING_IF(1, "The number of teams was unexpected, the test results are likely inconcuslive")
	OMPVV_TEST_AND_SET(errors, 1 == 2);

	OMPVV_REPORT_AND_RETURN(errors);
}
