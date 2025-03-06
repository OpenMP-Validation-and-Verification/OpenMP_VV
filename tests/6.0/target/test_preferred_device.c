//===--- test_preferred_device.c --------------------===//
//
// OpenMP API Version 6.0
// Tests that the preferred device memory allocator works
// as intended.
//
//===----------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include "ompvv.h"
#define N 12

int test_preferred_device() {
    int errors = 0;
    int device;
    int num_devices = omp_get_num_devices();

    if (num_devices > 0) {
        device = 0;
    } 
    else {
        device = omp_get_initial_device();
    }

    int *arr;

    omp_alloctrait_t trait = { omp_atk_preferred_device, device };
    omp_allocator_handle_t preferred_dev_alloc = omp_init_allocator(omp_default_mem_space, 1, &trait);

    arr = (int*)omp_alloc(N*sizeof(int),preferred_dev_alloc);

    #pragma omp target parallel for map(from: arr[0:N])
    for (int i = 0; i < N; i++) {
        arr[i] = i;
    }
    
    for (int i = 0; i < N; i++){
        OMPVV_TEST_AND_SET_VERBOSE(errors, arr[i] != i);
    }

    omp_free(arr, preferred_dev_alloc);
    omp_destroy_allocator(preferred_dev_alloc);

    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;

    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_preferred_device() != 0);
    OMPVV_REPORT_AND_RETURN(errors);
}
