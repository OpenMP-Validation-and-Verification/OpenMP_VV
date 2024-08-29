//===------ test_target_declare_induction.c ----------------------===//
//
// OpenMP API Version 6.0
//
// This test evaluates the declare induction directives using the induction
// clause, ensuring proper behavior and proper updates
//
//===-------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"
#include <math.h>

#define N 4

int test_target_declare_induction() {
        int errors = 0;
        int arr[N];
	int step_var = 2;
	int induction_var = 1;
	int expected = induction_var;	
	for (int i = 0; i < N; i++){
		arr[i] = 0;
	}

	#pragma declare induction(user_def_induc: int) inductor(omp_var=omp_var * 2 * omp_step) collector(pow(2 * omp_step, omp_idx))

        #pragma omp target parallel for induction(step(step_var), user_def_induc : induction_var) map(tofrom: a[:N])
        for (int i = 0; i < N; i++) {
                arr[i] = induction_var;
        }

	for (int i = 0; i < N; i++) {
        	expected *= 2 * step_var;  
        	OMPVV_TEST_AND_SET(errors, arr[i] != expected);
    	}

        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_declare_induction() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
