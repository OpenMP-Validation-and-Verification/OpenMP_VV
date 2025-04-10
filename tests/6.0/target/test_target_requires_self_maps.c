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

#define N 2

#pragma omp requires self_maps

int test_target_self_maps() {
    int errors = 0;
    int x[N];
    
    for(int i = 0; i < N; i++){
        x[i] = i;
    }	
	    
    #pragma omp target
    {
        for (int i = 0; i < N; i++){
	    x[i] += 9;
	}
    }

    for(int i = 0; i < N; i++){
        OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != i + 9);
    }
    
    for(int i = 0; i < N; i++){
        x[i] += 10;
    }
 
    #pragma omp target
    {
        for (int i = 0; i < N; i++){
            x[i] *= 2;
        }
    }

    for(int i = 0; i < N; i++){
        OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != (i + 19) * 2);
    }

    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_self_maps() != 0);
    OMPVV_REPORT_AND_RETURN(errors);
}
