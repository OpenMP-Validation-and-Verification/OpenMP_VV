//===------ test_target_parallel_for_reverse.c ------===//
//
// OpenMP API Version 6.0
//
// This test evaluates the reverse directive.
//
//===-------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10

int test_target_parallel_for_reverse() {
        int errors = 0;
	int arrayReverse[N];
        #pragma omp target parallel for reverse map(tofrom: arrayReverse[:N])
        for (int i = 0; i < N; i++) {
                arrayReverse[i] = N - i -1;
        }
        
	int expected = 0;
	for (int i = N - 1; i >= 0; i--){	
		OMPVV_TEST_AND_SET(errors, arrayReverse[i] != expected);
		expected++;
	}

        return errors;
}

int main() {
        OMPVV_TEST_OFFLOADING;
        int errors = 0;

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_parallel_for_reverse() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}
