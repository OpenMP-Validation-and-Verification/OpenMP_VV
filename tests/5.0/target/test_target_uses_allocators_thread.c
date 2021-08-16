//===--- test_target_uses_allocators_thread.c -----------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// The tests checks the uses_allocators clause with omp_thread_mem_alloc.
// The variable allaocated in the target region is modified and used to compute
// result. Result is copied back to the host and checked with computed value on host.
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 8

int test_uses_allocators_thread() {
  int errors = 0;
  int x = 0;
  int device_result[N] = {0};
  int result[N] = {0};

  for (int i = 0; i < N; i++) {
    result[i] = 3 * i ;
  }

#pragma omp target parallel uses_allocators(omp_thread_mem_alloc) allocate(omp_thread_mem_alloc: x) private(x) map(from: device_result)
{
  for (int i = 0; i < N; i++) {
    x = 2 * i;
    device_result[i] = i + x;
  }
}
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, result[i] != device_result[i]);
  }

  return errors;
}

int main() {

  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_uses_allocators_thread() != 0);

  OMPVV_REPORT_AND_RETURN(errors);
}
