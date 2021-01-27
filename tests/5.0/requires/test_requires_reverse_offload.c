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
    if (!omp_is_initial_device())
        {
            errors++;
        }

    return errors;
}

#pragma omp declare target device_type(host) to(host_function)

int main() 
{    
    int isOffloading;
    int errors;

    errors = 0;
   
    OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);

    OMPVV_WARNING_IF(!isOffloading, "Without offloading enabled, host execution is already guaranteed")

    int A[N];

    for (int i = 0; i < N; i++) 
    {
        A[i] = i;
    }

    A[N-1] = -1;

    #pragma omp target map (A) 
    {
        for (int i = 0; i < N; i++) 
        {
           if (A[i] != i) 
           {
                #pragma omp target device(ancestor:1) map(always, to: A[i:1])
                errors = host_function(A[i], i, errors);
           }
        }
    }

    OMPVV_REPORT_AND_RETURN(errors)
}


