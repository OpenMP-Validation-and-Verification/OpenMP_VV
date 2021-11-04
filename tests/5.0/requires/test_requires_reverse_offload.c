//===---test_requires_reverse_offload.c ---------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
// 
// This test checks to see that the reverse_offload clause on a requires directive 
// is supported, and if so, the function host_function() will made available as a 
// procedure only on the host. By specificying the 'ancestor' modifier with device 
// number of 1, we are indicating to compiler that execution is to be performed on
// the immediate parent, the host. The omp declare target statement ensures that the
// host_function will only be available on host.  
//
// Based on OpenMP 5.0 Example: target_reverse_offload.7.c
//===--------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

#pragma omp requires reverse_offload


int host_function(int incorrect_value, int index, int errors)
{     
    OMPVV_INFOMSG("Planned Error in offload: A[%d]=%d\n", index, incorrect_value);
    OMPVV_INFOMSG("                Expecting: A[i] =i\n");
    // Would like to use omp_is_initial_device() here, but calls to OpenMP runtime API
    // are not permitting inside target region when device(ancestor:1) is used
    // Instead, we will change values of the array A that was previously defined on the host
    // and will not explicity map back to host. If reverse offloading is truly enabled,
    // these values should be updated back on the host.
    // if (!omp_is_initial_device())
    //    {
    //        errors++;
    //    }

    for (int j = 0; j < N; j++) {
        A[j] = 2*j;
    }

    return errors;
}

#pragma omp declare target device_type(host) to(host_function)

int main() 
{    
    int A[N];
    int isOffloading;
    int errors;
    int device_num;

    errors = 0;
   
    OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);

    OMPVV_WARNING_IF(!isOffloading, "Without offloading enabled, host execution is already guaranteed")

    device_num = omp_get_num_devices();

    for (int i = 0; i < N; i++) {
        A[i] = i;
    }

    A[N-1] = -1;

    #pragma omp target enter data map(to: A) 

    if (device_num > 0) {
        for (int i = 0; i < N; i++) {
            if (A[i] != i) {
                #pragma omp target device(ancestor:1) map(always, to: A[i:1])
                errors = host_function(A[i], i, errors);
            }
        }
	for (int i = 0; i < N; i++) {
            OMPVV_TEST_AND_SET(errors, A[i] != 2*i);
	}
    }

    OMPVV_WARNING_IF(device_num <= 0, Cannot properly properly test reverse offload if no devices are available)

    OMPVV_REPORT_AND_RETURN(errors)
}


