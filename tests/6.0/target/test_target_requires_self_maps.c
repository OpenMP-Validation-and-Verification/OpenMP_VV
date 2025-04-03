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
    int x = 16;
    
    #pragma omp target
    {
        x += 4;
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 20);
    
    x *= 2;
    
    #pragma omp target
    {
        x += 10;
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 50);
    
    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_self_maps() != 0);
    OMPVV_REPORT_AND_RETURN(errors);
}
