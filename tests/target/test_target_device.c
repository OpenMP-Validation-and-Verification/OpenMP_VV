// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

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
