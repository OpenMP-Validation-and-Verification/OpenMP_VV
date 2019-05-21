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

#define SIZE_N 2000

int test_target_teams_distribute_parallel_for_map_default(int isShared) {
  OMPVV_INFOMSG("test_target_teams_distribute_parallel_for_devices");
  
  int a[SIZE_N];
  int b[SIZE_N];
  int c[SIZE_N];
  int d[SIZE_N];
  int scalar = 20;
  int scalar2 = -1;
  int errors = 0;
  int i, j, dev;

  // array initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
    b[i] = i;
    c[i] = 2*i;
    d[i] = 0;
  }


#pragma omp target teams distribute parallel for
  for (j = 0; j < SIZE_N; ++j) {
    // scalar is firstprivate for the target region, but 
    // in a parallel construct, if not default clause is present
    // the variable is shared. Hence scalar = any other value 
    // could cause a data race.
    // See page 182, line 1.
    d[j] += c[j] * (a[j] + b[j] + scalar);
    scalar2 = j;
  }

  if (!isShared)
    OMPVV_TEST_AND_SET(errors, scalar2 != -1);
  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, d[i] != (1 + i + 20) * 2*i);
  }

  return errors;
}

// Test for OpenMP 4.5 target enter data with if
int main() {
  int isSharedEnv, isOffloading;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_SHARED_ENVIRONMENT(isSharedEnv);
  int errors = 0;
  
  OMPVV_WARNING_IF(isSharedEnv, "This test is inconclusive on shared memory environments")

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_parallel_for_map_default(isSharedEnv));

  OMPVV_REPORT_AND_RETURN(errors);
}
