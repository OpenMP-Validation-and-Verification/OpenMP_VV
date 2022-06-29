//===--- test_target_memcpy_rect_async_depobj.c ----------------------------===//
//
//  Inspired from OpenMP 5.1 Examples Doc, 5.16.4 & 8.9
//  This test utilizes the omp_target_memcpy_rect_async construct to
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

#define N 5

int errors, i, j;

int test_target_memcpy_async_depobj() {

    int h, t;
    errors = 0;
    double *devRect;
    h = omp_get_initial_device();
    t = omp_get_default_device();

    double* hostRect[N];
    for(i = 0; i < N; i++){
        hostRect[i] = (double *)malloc( sizeof(double)*N); 
    } // 5x5 2D array
    devRect = (double *)omp_target_alloc( sizeof(double)*N, t);
    for(i = 0; i < N; i++){
        devRect[i] = (double *)omp_target_alloc( sizeof(double)*N, t);
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, devRect == NULL);

    for(i = 0; i < N; i++){
        for (j = 0; j < N; j++){
            hostRect[i][j] = i;
        }
    }
    omp_depend_t obj;
    #pragma omp depobj(obj) depend(inout: devRect)
    omp_depend_t obj_arr[1] = {obj};

    /* copy to device memory */
    omp_target_memcpy_rect_async(devRect, hostRect, 
                                32 /*number of bits??*/, 2, 5, //5 by 5
                                0,          0,
                                sizeof(double)*N*N, sizeof(double)*N*N,
                                t,          h,
                                1,          obj_arr);

    #pragma omp taskwait depend(depobj: obj)
    #pragma omp target is_device_ptr(devRect) device(t) depend(depobj: obj)
    {
        for(i = 0; i < N; i++){
            for (j = 0; j < N; j++){
                devRect[i][j] = devRect[i][j]*2; // initialize data
            }
        }
    }

    /* copy to host memory */
    omp_target_memcpy_rect_async(hostRect, devRect,
                                32, 2, 5
                                0,          0,
                                sizeof(double)*N*N, sizeof(double)*N*N,
                                h,          t,
                                1,          obj_arr);

    #pragma omp taskwait depend(depobj: obj)
    for(i = 0; i < N; i++){
        for(j = 0; j < N; j++){
            OMPVV_TEST_AND_SET(errors, hostRect[i][j]!=i*2);
        }
    }
    // free resources
    omp_target_free(devRect, t);
    #pragma omp depobj(obj) destroy
    return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_memcpy_async_depobj() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}
