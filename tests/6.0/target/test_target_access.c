//===--- test_target_access_single.c --------------------===//
//
// OpenMP API Version 6.0
// Tests that the target_access single allocator trait works
// as intended for basic device-host operations.
//
//===----------------------------------------------------===//
#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 16

int test_target_access_single() {
    int errors = 0;
    int *array;
    
    omp_alloctrait_t trait = { omp_atk_target_access, omp_atv_single };
    omp_allocator_handle_t single_allocator = omp_init_allocator(omp_default_mem_space, 1, &trait);
    
    array = (int*)omp_alloc(N * sizeof(int), single_allocator);
    
    for (int i = 0; i < N; i++) {
        array[i] = i;
    }
    
    #pragma omp target map(tofrom: array[:N])
    {
        for (int i = 0; i < N; i++) {
            array[i] = i * N ;
        }
    }
    
    for (int i = 0; i < N; i++) {
        OMPVV_TEST_AND_SET_VERBOSE(errors, array[i] != i * 10);
    }
    
    omp_free(array, single_allocator);
    omp_destroy_allocator(single_allocator);
    
    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;
    
    int errors = 0;
    
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_access_single() != 0);
    
    OMPVV_REPORT_AND_RETURN(errors);
}
