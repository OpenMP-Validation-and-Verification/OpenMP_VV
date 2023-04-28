//===------test_target_teams_allocate.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// testTargetTeamsAllocateCl()
// This is a basic test to demonstrate how a allocate can be used with
// target teams construct. Allocate clause is used to allocate a local
// variable of size int.
//
// testTargetTeamsAllocateArrayCl()
// This is a basic test to demonstrate how a allocate can be used with
// target teams construct. Allocate clause is used to allocate a local
// array.
//===------------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int testTargetTeamsAllocateCl() {
  int a[N], b[N], c[N], local;
  int errors = 0;
  int isDevice = 0;
  // Data Inititalize
  for (int i = 0; i < N; i++) {
    a[i] = 2*i;  // Even
    b[i] = 2*i + 1;  // Odd
    c[i] = 0;
  }
  // Execute on target
#pragma omp target teams map(to: a[0:N], b[0:N]) map(from: c[0:N]) map(tofrom: isDevice) \
        uses_allocators(omp_default_mem_alloc) \
        allocate(omp_default_mem_alloc: local) private(local)
  {
    isDevice = omp_is_initial_device();
    for (int i = 0; i < N; i++) {
      local = a[i] + b[i];
      c[i] = local;
    }
  }
  // Validate
  OMPVV_TEST_AND_SET_VERBOSE(errors, (isDevice != 0));
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, c[i] != a[i] + b[i]);
  }
  return errors;
}

int testTargetTeamsAllocateArrayCl() {
  int a[N], b[N], c[N], local[N];
  int errors = 0;
  int isDevice = 0;
  // Data Inititalize
  for (int i = 0; i < N; i++) {
    a[i] = 2*i;  // Even
    b[i] = 2*i + 1;  // Odd
  }
  printf("Before target omp_is_initial_device: %d\n", omp_is_initial_device());
  // Execute on target
#pragma omp target teams map(to: a[0:N], b[0:N]) map(from: c[0:N]) map(tofrom: isDevice) \
uses_allocators(omp_default_mem_alloc) allocate(omp_default_mem_alloc: local) private(local)
  {
    isDevice = omp_is_initial_device();
    for (int i = 0; i < N; i++) {
      local[i] = a[i] + b[i];
    }
    for (int i = 0; i < N; i++) {
      c[i] = local[i];
    }
  }
  printf("During target omp_is_initial_device: %d\n", isDevice);
  // Validate
  OMPVV_TEST_AND_SET_VERBOSE(errors, (isDevice != 0));
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, c[i] != a[i] + b[i]);
  }
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsAllocateCl());
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsAllocateArrayCl());
  OMPVV_REPORT_AND_RETURN(errors);
}
