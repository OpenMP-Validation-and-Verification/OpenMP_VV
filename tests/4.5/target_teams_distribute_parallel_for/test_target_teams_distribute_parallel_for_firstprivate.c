//===---- test_target_teams_distribute_parallel_for_firstprivate.c - combined consutrct -===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test check for a private variable within a pragma omp target teams distribute 
// parallel for that is initialized from the host through firstprivate clause
// We use a private variable within a for loop and asign it every iteration
// hoping that we won't get into data races. We do this multiple times to improve
// test
//
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>

#define SIZE_N 1024

int test_target_teams_distribute_parallel_for_firstprivate() {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_firstprivate");
  
  int a[SIZE_N];
  int b[SIZE_N];
  int c[SIZE_N];
  int d[SIZE_N];
  int privatized = 10;
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
#pragma omp target data map(to: a[0:SIZE_N], b[0:SIZE_N], c[0:SIZE_N]) map(from: d[0:SIZE_N])
  {
#pragma omp target teams distribute parallel for firstprivate(privatized)
      for (j = 0; j < SIZE_N; ++j) {
        for (i = 0; i < a[j] + b[j]; ++i) {
          privatized++;
        }
        d[j] = c[j] * privatized;
      }
  }

  for (i = 0; i < SIZE_N; i++) {
    // 10 = initial value of privatized + 1 initial value of a[i] 
    // + i initial value of b[i]
    OMPVV_TEST_AND_SET(errors, d[i] != (10 + 1 + i)*2*i);
  }

  return errors;
}

// Test for OpenMP 4.5 target enter data with if
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_firstprivate());

  OMPVV_REPORT_AND_RETURN(errors);
}
