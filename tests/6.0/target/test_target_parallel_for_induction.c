//===------ test_target_parallel_for_induction.c ------------------===//
//
// OpenMP API Version 6.0
//
// This test evaluates the parallel for directives using the induction
// clause, ensuring proper behavior and proper updates
//
//===-------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 1024

int test_parallel_for_induction() {
        int errors = 0;
        int arr[N];
	int x;
	// Initialize array
	for (int i = 0; i < N; i++){
		arr[i] = i;
	}

        #pragma omp target parallel for induction(x) map(tofrom: a[0:N])
        for (int i = 0; i < N; i++) {
		x = i;
                arr[i] += x;
        }

	for (int i = 0; i < N; i++){
		OMPVV_TEST_AND_SET(errors, arr[i] != i*2);
	}

        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_parallel_for_induction() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
