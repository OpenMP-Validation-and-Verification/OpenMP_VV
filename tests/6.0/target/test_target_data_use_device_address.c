//===------ test_target_data_use_device_address.c ----------------------===//
//
// OpenMP API Version 6.0
//
// This test evaluates the use_device_addr with an assumed-size array
// to ensure proper behavior on the device.
//
//===------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"
#include <math.h>

#define N 8

int test_target_data_use_device_address() {
    int errors = 0;
    int array[N];
    int *pointer;

    for (int i = 0; i < N; i++){
        array[i] = 0;
    }

    pointer = &array[0];

    #pragma omp target data map(tofrom: array[:])
    {
        int *device_addr;

        #pragma omp target data use_device_addr(pointer[:])
	{
            device_addr = pointer;
		
	    #pragma omp target has_device_addr(pointer[:N])
	    {
                for (int i = 0; i < N; i++){
                    device_addr[i] = 1;
        	}
	    }
	}
    }
	
    for (int i = 0; i < N; i++){
        OMPVV_TEST_AND_SET_VERBOSE(errors, array[i] != 1);
    }
    
    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_data_use_device_address() != 0);
    OMPVV_REPORT_AND_RETURN(errors);
}
