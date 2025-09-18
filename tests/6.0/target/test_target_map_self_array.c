//===---------------------- test_target_map_self_array.c ---------------------------===//
//
// OpenMP API Version 6.0
// Tests the target directive with the map clause and self map-type modifier to
// verify proper behavior of the self map-type modifier with arrays. This test
// confirms that the memory address is identical on both host and device (true
// shared memory), data modifications on the device are visible on the host and
// vice versa, and the self modifier correctly enables direct access to the same
// storage without data copying between host and device.
//
////===----------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 2

#pragma omp requires unified_shared_memory

int test_target_map_self_array(){
    int errors = 0;
    int arr[N];
    int* host_pointer = arr;
    int* device_pointer = NULL;

    for(int i = 0; i < N; i++){
        arr[i] = i;
    }

    #pragma omp target map(self: arr) map(from: device_pointer)
    {
        for(int i = 0; i < N; i++){
            arr[i] *= 2;
        }

        device_pointer = arr;
    }

    for(int i = 0; i < N; i++){
        arr[i] *= 10;
    }

    for(int i = 0; i < N; i++){
        OMPVV_TEST_AND_SET_VERBOSE(errors, arr[i] != i * 2 * 10);
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, host_pointer != device_pointer);

    return errors;
}

int main(){
        OMPVV_TEST_OFFLOADING;
        int errors = 0;
        OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_map_self_array() != 0);
        OMPVV_REPORT_AND_RETURN(errors);
}

