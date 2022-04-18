//===--- test_target_has_device_addr.c --------------------------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test verifies the 'has_device_addr' feature added to the target construct.
// We tested this by mapping a scalar & array to the device
// and ensuring that the address does not change when using
// has_device_addr on another target region.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

//test by mapping to device use 'target map' construct
int test_target_has_device_addr() {
  OMPVV_INFOMSG("test_target_has_device_addr");
  int errors = 0;
  int x = 10;
  int arr[N];
  for(int i=0;i<N;i++){
    arr[i] = i;
  }
  int *first_scalar_device_addr;
  int *first_arr_device_addr;
  #pragma omp target enter data map(to: x, arr)
  #pragma omp target map(from: first_scalar_device_addr, first_arr_device_addr) map(to: x, arr)
  {
    first_scalar_device_addr = &x;
    first_arr_device_addr = &arr[0];
  }
  int *second_scalar_device_addr, *second_arr_device_addr;
  //check addresses are same on device region
  #pragma omp target data use_device_addr(x, arr)
  #pragma omp target map(from:second_scalar_device_addr, second_arr_device_addr) has_device_addr(x, arr) 
  {
    second_scalar_device_addr = &x;
    second_arr_device_addr = &arr[0];
  }
  #pragma omp target exit data map(release: x, arr)
  OMPVV_TEST_AND_SET(errors, first_scalar_device_addr != second_scalar_device_addr);
  OMPVV_TEST_AND_SET(errors, first_arr_device_addr != second_arr_device_addr)
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_has_device_addr());
  OMPVV_REPORT_AND_RETURN(errors);
}
