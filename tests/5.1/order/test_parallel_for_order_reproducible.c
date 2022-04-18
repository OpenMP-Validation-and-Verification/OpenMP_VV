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
	int x[N];
	int y[N];

	for (int i = 0; i < N; i++) {
		x[i] = 0;
		y[i] = 0;
	}

	OMPVV_TEST_OFFLOADING;

	#pragma omp target map(tofrom: x,y)
	{

	#pragma omp parallel for order(concurrent)
	for (int i = 0; i < N; i++) {
		x[i] = omp_get_thread_num();	
	}

	#pragma omp parallel for order(concurrent)
	for (int i = 0; i < N; i++) {
                y[i] = omp_get_thread_num();
        }

	}

	for (int i = 0; i < N; i++) {
        	OMPVV_TEST_AND_SET(errors, x[i] != y[i]);
       	}

	OMPVV_REPORT_AND_RETURN(errors);
}
