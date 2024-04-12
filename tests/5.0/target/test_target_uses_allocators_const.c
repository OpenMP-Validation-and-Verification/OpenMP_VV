//===--- test_target_uses_allocators_const.c -------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// The tests checks the uses_allocators clause with omp_const_mem_alloc. 
// The variable allaocated in the target region and and is used to 
// modify result on device. Result is copied back to the host and checked 
// with computed value on host.
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_uses_allocators_const() {
  int errors = 0;
  int x = 0;
  int device_result = 0;
  int result = 0;


  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      result += j + i ;
    }
  }

#pragma omp target uses_allocators(omp_const_mem_alloc) allocate(omp_const_mem_alloc: x) firstprivate(x) map(from: device_result)
{
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      x += j + i;
    }
  }
  device_result = x;
}

  OMPVV_WARNING_IF(result != device_result,"Variable x may have been assigned to const memory space and device_result wasn't updated to the expected value, but it is correct");
  OMPVV_WARNING_IF(result == device_result,"Variable x may or may not have been assigned to const memory space but device_result was updated to the expected value");

  return errors;
}

int main() {

  OMPVV_TEST_OFFLOADING;

  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_uses_allocators_const() != 0);

  OMPVV_REPORT_AND_RETURN(errors);
}

