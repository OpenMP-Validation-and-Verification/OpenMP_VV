//===--- test_target_device.c ----------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the target construct with device clause where device-
// modifier is either ancestor or device_num. If no device_modifier is 
// present, the behavior is the same as if device_num were present.
//
////===---------------------------------------------------------------------===//

#include <stdlib.h>
#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 1024

// Required for 'device(ancestor: 1)
#pragma omp requires reverse_offload

int test_target_device_ancestor() {

    int which_device;
    int a[N];
    int errors = 0; 
    int is_shared_env = 0;
    which_device = 0;

    for (int i = 0; i < N; i++) {
        a[i] = i;
    }

    OMPVV_TEST_AND_SET(errors, omp_get_num_devices() <= 0);
    OMPVV_WARNING_IF(omp_get_num_devices() <= 0, "[SKIPPED] Since no target devices were found, this test"
                                                 "will be skipped.");
    OMPVV_TEST_AND_SET_SHARED_ENVIRONMENT(is_shared_env);
    OMPVV_WARNING_IF(is_shared_env != 0, "[WARNING] target_device_ancestor() test may not be able to detect errors if the target system supports shared memory.")

    #pragma omp target //Run on the default device, which is the host for device_num = 0
    {

        #pragma omp target device(ancestor: 1) map(tofrom: a) map(to: which_device) 
	{
	    for (int i = 0; i < N; i++) {
                a[i] = a[i] + 2;
	    }
	    // For ancestor, the spec mandates: "No OpenMP constructs or calls to
	    // OpenMP API runtime routines are allowed":
	    // which_device = omp_is_initial_device();
	    // Instead, check that scalar is mapped back properly after exiting target region
	    which_device = 75;
	}
    }

    OMPVV_ERROR_IF(which_device != 75, "Target region was executed on a target device. Due to ancestor device-modifier,"
                                         "this region should execute on a host device");


    return errors;

}

int test_target_device_device_num() {
    
    int target_device_num, host_device_num, first_device_num;
    int b[N];
    int errors = 0; 

    for (int i = 0; i < N; i++) {
        b[i] = i;
    }

    host_device_num = omp_get_device_num(); 
    target_device_num = host_device_num;

    
    OMPVV_TEST_AND_SET(errors, omp_get_num_devices() <= 0);
    OMPVV_WARNING_IF(omp_get_num_devices() <= 0, "[SKIPPED] Since no target devices were found, this test"
                                                 "will be skipped");
	
    if (omp_get_num_devices() > 0) {
         
        first_device_num = omp_get_num_devices() - 1;
        #pragma omp target device(device_num: first_device_num) map(tofrom: b, target_device_num) 
        {
            for (int i = 0; i < N; i++) {
                b[i] = b[i] + 2;
            }
 
            target_device_num = omp_get_device_num();
        }

        OMPVV_ERROR_IF(target_device_num == host_device_num, "Target region was executed on host," 
                   "this region should execute on specified target device number");   

    }

    OMPVV_TEST_AND_SET(errors, target_device_num == host_device_num);

    return errors;

}

int main() {

    int errors = 0;
   
    OMPVV_TEST_OFFLOADING;

    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_device_ancestor());
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_device_device_num());

    OMPVV_REPORT_AND_RETURN(errors);
}
