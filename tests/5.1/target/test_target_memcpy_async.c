//===--- test_target_memcpy_async.c -----------------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  Inspired from OpenMP 5.1 Examples Doc, 5.16.4 & 8.9
//  This test utilizes the omp_target_memcpy_async construct to
//  allocate memory on the device asynchronously. The construct
//  uses 'obj' for dependency, so that memory is only copied once
//  the x variable is changed
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors, i;

int test_target_memcpy_async() {

    int h, t, i, x;
    errors = 0;
    double *mem;
    double *mem_dev_cpy;
    *mem = 10; 
    h = omp_get_initial_device();
    t = omp_get_default_device();

    OMPVV_TEST_AND_SET_VERBOSE(errors, omp_get_num_devices() < 1 || t < 0);

    mem = (double *)malloc( sizeof(double)*N);
    mem_dev_cpy = (double *)omp_target_alloc( sizeof(double)*N, t);

    OMPVV_TEST_AND_SET_VERBOSE(errors, mem_dev_cpy == NULL);

    omp_depend_t obj;

    #pragma omp depobj(obj) depend(inout: x)

    /* dst src */
    omp_target_memcpy_async(mem_dev_cpy, mem, sizeof(double)*N,
                                0,          0,
                                t,          h,
                                1,          *obj);

    #pragma omp target is_device_ptr(mem_dev_cpy) device(t)
    #pragma omp teams distribute parallel for
    for(i = 0;i<N;i++){
        mem_dev_cpy[i] = cos((double)i); // initialize data
    }
    /* dst src */
    omp_target_memcpy_async(mem, mem_dev_cpy, sizeof(double)*N,
                                0,          0,
                                h,          t,
                                1,          *obj);

    for(int i=0;i<N;i++){
        OMPVV_TEST_AND_SET(errors, mem_dev_cpy[i]!=cos((double)i));
    }
    omp_target_free(mem_dev_cpy, def_dev);
    #pragma omp depobj(obj) destroy // frees resource
    return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_memcpy_async() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}           
