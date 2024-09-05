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
#include <math.h>

#define N 8

int test_target_parallel_for_induction() {
        int errors = 0;
        int arr[N];
	int step_var = 2;
	int induction_var = 1;
	
	for (int i = 0; i < N; i++){
		arr[i] = 0;
	}

        #pragma omp target parallel for induction(step(step_var), *: induction_var) map(tofrom: arr[:N])
        for (int i = 0; i < N; i++) {
                arr[i] = i + induction_var;
		induction_var *= step_var;
        }

	for (int i = 0; i < N; i++){
		OMPVV_TEST_AND_SET(errors, arr[i] != i + pow(step_var,i));
	}

        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_parallel_for_induction() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
