//===---- test_target_teams_distribute_parallel_for_map_default.c - combined consutrct -===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// test the mapping of arrays by default. The expected behavior is that all the arrays are
// mapped tofrom by default
//
//===-----------------------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>

#define SIZE_N 1024
#define ITERATIONS 1000

int test_target_teams_distribute_parallel_for_map_default() {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_devices");
  
  int a[SIZE_N];
  int b[SIZE_N];
  int c[SIZE_N];
  int d[SIZE_N];
  int errors = 0;
  int i, j, dev;

  // array initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
    b[i] = i;
    c[i] = 2*i;
    d[i] = 0;
  }

  // check multiple sizes. 
  for (i = 0; i < ITERATIONS; ++i) {
#pragma omp target teams distribute parallel for
    for (j = 0; j < SIZE_N; ++j) {
      d[j] = c[j] * (a[j] + b[j]);
    }
  }

  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, d[i] != (1 + i)*2*i);
  }

  return errors;
}

// Test for OpenMP 4.5 target enter data with if
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_map_default());

  OMPVV_REPORT_AND_RETURN(errors);
}
