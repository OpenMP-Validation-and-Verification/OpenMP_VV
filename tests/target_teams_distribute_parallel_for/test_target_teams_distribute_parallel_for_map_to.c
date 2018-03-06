//===---- test_target_teams_distribute_parallel_for_map_default.c - combined consutrct -===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// testing the maping to of arrays and a non-variable. Making sure that it does not copy it 
// over 
//
//===-----------------------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>

#define SIZE_N 2000
#define ITERATIONS 1000

int test_target_teams_distribute_parallel_for_map_to() {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_map_to");
  
  int a[SIZE_N];
  int b[SIZE_N];
  int c[SIZE_N];
  int d[SIZE_N];
  int scalar = 50; // This one is to test the to of an scalar
  int scalar2 = 60; // This one is to test that it does not copy back from device
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
#pragma omp target teams distribute parallel for map(to: a,b,c, scalar) map(tofrom: d)
    for (j = 0; j < SIZE_N; ++j) {
      d[j] = c[j] * (a[j] + b[j] + scalar);
      if (!omp_is_initial_device()) {
        scalar2 = -1;
        a[i] = -1;
        b[i] = -1;
        c[i] = -1;
      }
    }
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, scalar2 != 60);
  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, a[i] != 1);
    OMPVV_TEST_AND_SET(errors, b[i] != i);
    OMPVV_TEST_AND_SET(errors, c[i] != 2*i);
    OMPVV_TEST_AND_SET(errors, d[i] != (1 + i + 50)*2*i);
  }

  return errors;
}

// Test for OpenMP 4.5 target enter data with if
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_map_to());

  OMPVV_REPORT_AND_RETURN(errors);
}
