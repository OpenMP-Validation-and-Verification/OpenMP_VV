//===------test_target_teams_is_device_ptr.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// This is a basic test to demonstrate
// clause with target teams construct.
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

/**
  This is a basic test to demonstrate 
  clause with target teams construct.
*/
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  int device_data[N], host_data[N];
  for (int i = 0; i < N; i++) {
    device_data[i] = i;
    host_data[i] = 0;
  }
#pragma omp target data map(to: device_data[0:N])
  {
    int *dev_ptr;
#pragma omp target data use_device_addr(device_data)
    {
      dev_ptr = device_data;
    }
#pragma omp target teams map(from: host_data[0:N]) is_device_ptr(dev_ptr)
    {
      for (int i = 0; i < N; i++) {
        host_data[i] = dev_ptr[i];
      }
    }
  } // end target data
  // checking results
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, host_data[i] != device_data[i]);
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
