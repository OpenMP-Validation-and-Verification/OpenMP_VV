//===--- test_target_memcpy_async_depobj.c ----------------------------===//
//
//  Inspired from OpenMP 5.1 Examples Doc, 5.16.4 & 8.9
//  This test utilizes the omp_target_memcpy_async construct to
//  allocate memory on the device asynchronously. The construct
//  uses 'obj' for dependency, so that memory is only copied once
//  the variable listed in the depend clause is changed.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors, i;

int test_target_memcpy_async_depobj() {

    int h, t, i;
    errors = 0;
    double *mem;
    double *mem_dev_cpy;
    h = omp_get_initial_device();
    t = omp_get_default_device();


    mem = (double *)malloc( sizeof(double)*N);
    mem_dev_cpy = (double *)omp_target_alloc( sizeof(double)*N, t);

    OMPVV_TEST_AND_SET_VERBOSE(errors, mem_dev_cpy == NULL);

    for(i = 0; i < N; i++){
        mem[i] = i;
    }
    omp_depend_t obj;
    #pragma omp depobj(obj) depend(inout: mem_dev_cpy)
    omp_depend_t obj_arr[1] = {obj};

    /* copy to device memory */
    omp_target_memcpy_async(mem_dev_cpy, mem, sizeof(double)*N,
                                0,          0,
                                t,          h,
                                1,          obj_arr[0]);

    #pragma omp taskwait depend(depobj: obj)
    #pragma omp target is_device_ptr(mem_dev_cpy) device(t) depend(depobj: obj)
    for(i = 0; i < N; i++){
        mem_dev_cpy[i] = mem_dev_cpy[i]*2; // initialize data
    }

    /* copy to host memory */
    omp_target_memcpy_async(mem, mem_dev_cpy, sizeof(double)*N,
                                0,          0,
                                h,          t,
                                1,          obj_arr);

    #pragma omp taskwait depend(depobj: obj)
    for(int i=0; i < N; i++){
        OMPVV_TEST_AND_SET(errors, mem[i]!=i*2);
    }
    // free resources
    omp_target_free(mem_dev_cpy, t);
    #pragma omp depobj(obj) destroy
    return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_memcpy_async_depobj() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}
