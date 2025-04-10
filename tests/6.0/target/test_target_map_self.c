//===---------------------- test_target_map_self.c ---------------------------===//
//
// OpenMP API Version 6.0
// Tests the target directive with map(self:) clause to verify proper behavior of the
// self map-type modifier with scalar variables. This test confirms that the memory 
// address is identical on both host and device, and that data modifications are 
// visible across both environments without data copying, demonstrating correct 
// shared memory operation in a unified shared memory environment.
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#pragma omp requires unified_shared_memory

int test_target_map_self(){
    int errors = 0;
    int var = 1;
    int* host_pointer = &var;
    int* device_pointer = NULL;

    #pragma omp target map(self: var) map(from: device_pointer)
    {
        var += 2;
        device_pointer = &var;
    }

    var *= 10;
    
    OMPVV_TEST_AND_SET_VERBOSE(errors, var != 30);
   
    OMPVV_TEST_AND_SET_VERBOSE(errors, host_pointer != device_pointer);
    return errors;
}

int main(){
        OMPVV_TEST_OFFLOADING;
        int errors = 0;
        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_map_self() != 0);
        OMPVV_REPORT_AND_RETURN(errors);
}

