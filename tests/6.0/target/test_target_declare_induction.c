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

#define N 16

#pragma omp declare target 
int inductor_func(int inductor_var, int step) {
	return inductor_var * 2 * step;
}
#pragma omp end declare target
int test_target_declare_induction() {
        int errors = 0;
        int arr[N];
	int step_var = 2;
	int induction_var = 1;
	
	for (int i = 0; i < N; i++){
		arr[i] = 0;
	}

	#pragma declare induction(triple: int) inductor(omp_var=inductor_func(omp_var, omp_step) collector(omp_out)

        #pragma omp target parallel for induction(step(step_var), triple : induction_var) map(tofrom: a[:N])
        for (int i = 0; i < N; i++) {
                arr[i] = induction_var;
        }

	for (int i = 0; i < N; i++){
		OMPVV_TEST_AND_SET(errors, arr[i] != induction_var * 2 * step_var);
	}

        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_declare_induction() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
