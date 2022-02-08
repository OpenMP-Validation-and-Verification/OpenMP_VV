//===--- test_target_has_device_addr.c --------------------------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test verifies the 'has_device_addr' feature added to the target construct.
// We tested this by checking a scalar & array address in one target region, and
// verifying that the address remains the same in a second target region which
// uses the 'has_device_addr' feature.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

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
  int *second_scalar_device_addr;
  int *second_arr_device_addr;
  #pragma omp target map(from: first_scalar_device_addr, first_arr_device_addr)
  {
    first_scalar_device_addr = &x;
    first_arr_device_addr = &arr[0];
  }
  #pragma omp target map(from:second_scalar_device_addr, second_arr_device_addr) has_device_addr(x, arr[N]) 
  {
    second_scalar_device_addr = &x;
    second_arr_device_addr = &arr[0];
  }
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

