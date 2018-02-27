// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===---- test_target_teams_distribute_parallel_for.c - combined consutrct -===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// Testing
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>

#define SIZE_N 1024

int test_target_teams_distribute_parallel_for_devices() {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_devices");
  
  int num_dev = omp_get_num_devices();
  int a[SIZE_N];
  int errors = 0;
  int i, dev;

  OMPVV_WARNING_IF(num_dev <= 1, "Testing devices clause without multiple devices");

  // a and b array initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
  }

  for (dev = 0; dev < num_dev; ++dev) {
#pragma omp target enter data map(to: a[0:SIZE_N]) device(dev)
  }

  // check multiple sizes. 
  for (dev = 0; dev <= num_dev; ++dev) {
#pragma omp target teams distribute parallel for device(dev) \
    map (alloc: a)
    for (i = 0; i < SIZE_N; i++) {
      a[i]++;
    }
  }

  for (dev = 0; dev < num_dev; ++dev) {
#pragma omp target exit data map(from: a[0:SIZE_N]) device(dev)
    for (i = 0; i < SIZE_N; i++) {
      OMPVV_TEST_AND_SET(errors, a[i] != 2);
    }
  }

  return errors;
}

// Test for OpenMP 4.5 target enter data with if
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_devices());

  OMPVV_REPORT_AND_RETURN(errors);
}
