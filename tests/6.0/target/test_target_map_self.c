//===---------------------- test_target_map_self.c ---------------------------===//
//
// OpenMP API Version 6.0
// Tests the target directive with the map clause using the self-modifier to ensure
// proper behavior.
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"


int test_target_map_self(){
    int errors = 0;
    int var = 1;
    int* host_pointer = &var;
    int* device_pointer = NULL;

    #pragma omp target map(self: var) map(from: device_pointer)
    {
            device_pointer = &var;
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, host_pointer != device_pointer);
    return errors;
}

int main(){
        OMPVV_TEST_OFFLOADING;
        int errors = 0;
        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_map_self() != 0);
        OMPVV_REPORT_AND_RETURN(errors);
}

