//===--- test_target_map_devices.c --- target map to multiple devces ---------===//
//
// OpenMP API Version 4.5 Nov 2015
//
//  This test checks for data mapping on multiple devices when using the target
//  directive. It makes sure that data mapping is happening on each device
//  through the use of omp_set_default_device as well as the device() clause
//
//  Since OpenMP 4.5 does not have an API call to obtain the current device, 
//  this test does not guarantee that the execution devices are different. 
//  
//  Last modified by Jose M Monsalve Diaz, December 25, 2019
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

int test_target_device_clause() {
  OMPVV_INFOMSG("test_target_device_clause");

  // Get number of devices
  int num_dev = omp_get_num_devices();
  OMPVV_INFOMSG("num_devices tested = %d", num_dev);
  
  int array[N];
  int errors = 0;
  
  // Array initialization
  for (int i = 0; i < N; ++i) {
    array[i] = -1;
  }

  // Map the same array to multiple devices. initialize with device number
  for (int dev = 0; dev < num_dev; ++dev) {
#pragma omp target data map(tofrom: array[0:N]) device(dev)
    { 
    #pragma omp target map(alloc: array[0:N]) device(dev)
      {
        for (int i = 0; i < N; ++i) {
          array[i] += dev + 1;
        }
      } // end of omp target 
    } // end of omp target data
    for (int i = 0; i < N; ++i) {
      OMPVV_TEST_AND_SET(errors, array[i] != dev);
      array[i] = -1;
    }
  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_device_clause());

  OMPVV_REPORT_AND_RETURN(errors);
}
