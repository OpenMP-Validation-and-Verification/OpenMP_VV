//===------ test_target_reverse.c ----------------===//
//
// OpenMP API Version 6.0
//
// This test evaluates the reverse directive, ensuring
// proper behavior.
//
//===--------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 1024

int test_target_reverse() {
        int errors = 0;
        int arrayReverse[N];
        #pragma omp target map(tofrom: arrayReverse[:N])
        {
                int counter = 0;
		#pragma omp reverse
                for (int i = 0; i < N; i++) {
                  arrayReverse[i] = counter;
                  counter++;
                }

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

        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_reverse() != 0);

        OMPVV_REPORT_AND_RETURN(errors);
}

