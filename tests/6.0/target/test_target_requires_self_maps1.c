//===-------------- test_target_self_maps.c --------------===//
//
// OpenMP API Version 6.0
// Tests that the requires self maps directive allows updates between
// host and the device using unified memory. Verifies changes made
// without explicit mapping to ensure unified memory is used.
//
//===----------------------------------------------------===//
#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#pragma omp requires self_maps

int test_target_self_maps() {
    int errors = 0;
    int x[2];
    
    x[0] = 0;
    x[1] = 1; 
	    
    #pragma omp target
    {
        x[0] += 10;
	x[1] += 10;
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, x[0] != 10);
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[1] != 11);

    x[0] += 5;
    x[1] += 5;

    #pragma omp target
    {
        x[0] *= 2;
        x[1] *= 2;
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, x[0] != 30);
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[1] != 32);

    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_self_maps() != 0);
    OMPVV_REPORT_AND_RETURN(errors);
}
