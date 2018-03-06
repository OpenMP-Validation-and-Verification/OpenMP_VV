//===---- test_target_data_if.c - check the if clause of target data ------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// In this test we want to try to control if the if clause is working 
// when used with the combined construct target teams distribute parallel for
// To do this we check if offloading is working, if it is not, it won't be
// possible for us to tell if the test passed or not, since it depends on 
// offloading capabilities. 
//
// The if has a directive-name-modifier that specifies to which directive the 
// if applies to (either no directive, to the target or to the parallel). 
// we create three test, one for no directive, another one for the target directive
// and another one for the parallel directive 
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"
#include <stdio.h>

#define ATTEMPT_THRESHOLD 5
#define NUM_ATTEMPTS 10
#define SIZE_N 1024

int test_target_teams_distribute_if_no_modifier() {
  OMPVV_INFOMSG("test_target_teams_distribute_if_no_modifier");
  
  int isOffloading = 0;
  int a[SIZE_N], init_num_threads_dev[SIZE_N], init_num_threads_host[SIZE_N];
  int attempt = 0;
  int errors = 0;
  int i;

  // We test for offloading
#pragma omp target map(from: isOffloading)
  {
    isOffloading = !omp_is_initial_device();
  }
  
  OMPVV_WARNING_IF(!isOffloading, "With offloading off, it is not possible to test if");

  // a, init_num_threads_dev and init_num_threads_host arrays initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
    init_num_threads_dev[i] = 0;
    init_num_threads_host[i] = 0;
  }

  // Get the init_num_threads for host and device. If it is 1, then we 
  // cannot test the if parallel 
  // See section 2.5.1 of the ref manual
#pragma omp target teams distribute parallel for num_threads(10)
  for (i = 0; i < SIZE_N; i++) {
    init_num_threads_dev[i] = omp_get_num_threads();
  }

#pragma omp parallel for num_threads(10)
  for (i = 0; i < SIZE_N; i++) {
    init_num_threads_host[i] = omp_get_num_threads();
  }
  
  // We check that none of the values is 1 or less. This would make the test of if target
  // not possible
  int raiseWarningDevice = 0, raiseWarningHost = 0;
  for (i = 0; i < SIZE_N; i++) {
    if (init_num_threads_dev[i] <= 1 ) {
      raiseWarningDevice = 1;
    }
    if ( init_num_threads_host[i] <= 1) {
      raiseWarningHost = 1;
    }
  }
  OMPVV_WARNING_IF(raiseWarningDevice, "Initial number of threads in device was 1. It is not possible to test the if for parallel");
  OMPVV_WARNING_IF(raiseWarningHost, "Initial number of threads in host was 1. It is not possible to test the if for parallel");

  // We tests multiple times. Each one we check if we are running on the initial device or not
  // if we are not we increase the value by 1, and we check if the num_threads is different
  // to the one meassured before. If we are running on the host, the if should also affect the 
  // parallel clause and we should have the number of threads = 1
  for (attempt = 0; attempt < NUM_ATTEMPTS; ++attempt) {
#pragma omp target teams distribute parallel for if(attempt >= ATTEMPT_THRESHOLD)\
    map(tofrom: a) num_threads(10)
    for (i = 0; i < SIZE_N; i++) {
      if (omp_is_initial_device()) {
        a[i] += (omp_get_num_threads() != 1) ? 10 : 0; // This +10 should not happen
      } else {
        a[i] += (omp_get_num_threads() != 1 ? 1 : 0); 
      }
    }
  }

  // a and b array initialization
  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, a[i] != 1 + (NUM_ATTEMPTS - ATTEMPT_THRESHOLD));
  }

  OMPVV_ERROR_IF(errors, "Unexpected value after if clause");

  return errors;
}

int test_target_teams_distribute_if_target_modifier() {
  OMPVV_INFOMSG("test_target_teams_distribute_if_target_modifier");
  
  int isOffloading = 0;
  int a[SIZE_N], init_num_threads_dev[SIZE_N], init_num_threads_host[SIZE_N];
  int attempt = 0;
  int errors = 0;
  int i;

  // We test for offloading
#pragma omp target map(from: isOffloading)
  {
    isOffloading = !omp_is_initial_device();
  }
  
  OMPVV_WARNING_IF(!isOffloading, "With offloading off, it is not possible to test if");

  // a, init_num_threads_dev and init_num_threads_host arrays initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
    init_num_threads_dev[i] = 0;
    init_num_threads_host[i] = 0;
  }

  // Get the init_num_threads for host and device. If it is 1, then we 
  // cannot test the if parallel 
  // See section 2.5.1 of the ref manual
#pragma omp target teams distribute parallel for num_threads(10)
  for (i = 0; i < SIZE_N; i++) {
    init_num_threads_dev[i] = omp_get_num_threads();
  }

#pragma omp parallel for num_threads(10)
  for (i = 0; i < SIZE_N; i++) {
    init_num_threads_host[i] = omp_get_num_threads();
  }
  
  // We check that none of the values is 1 or less. This would make the test of if target
  // not possible
  int raiseWarningDevice = 0, raiseWarningHost = 0;
  for (i = 0; i < SIZE_N; i++) {
    if (init_num_threads_dev[i] <= 1 ) {
      raiseWarningDevice = 1;
    }
    if (init_num_threads_host[i] <= 1) {
      raiseWarningHost = 1;
    }
  }
  OMPVV_WARNING_IF(raiseWarningDevice, "Initial number of threads in device was 1. It is not possible to test the if for parallel");
  OMPVV_WARNING_IF(raiseWarningHost, "Initial number of threads in host was 1. It is not possible to test the if for parallel");

  // We tests multiple times. Each one we check if we are running on the initial device or not
  // if we are not we increase the value by 1, and we check if the num_threads is different
  // to the one meassured before. If we are running on the host, the if should also affect the 
  // parallel clause and we should have the number of threads = 1
  for (attempt = 0; attempt < NUM_ATTEMPTS; ++attempt) {
#pragma omp target teams distribute parallel for if(target: attempt >= ATTEMPT_THRESHOLD)\
    map(tofrom: a) num_threads(10)
    for (i = 0; i < SIZE_N; i++) {
      if (omp_is_initial_device()) {
        a[i] += (omp_get_num_threads() == 1) ? 10 : 0; // This +10 should not happen
      } else {
        a[i] += (omp_get_num_threads() != 1 ? 1 : 0);
      }
    }
  }

  // a and b array initialization
  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, a[i] != 1 + (NUM_ATTEMPTS - ATTEMPT_THRESHOLD));
  }

  OMPVV_ERROR_IF(errors, "Unexpected value after if(target: ...) clause");

  return errors;
}

int test_target_teams_distribute_if_parallel_modifier() {
  OMPVV_INFOMSG("test_target_teams_distribute_if_parallel_modifier");
  
  int isOffloading = 0;
  int a[SIZE_N], init_num_threads_dev[SIZE_N], init_num_threads_host[SIZE_N];
  int attempt = 0;
  int errors = 0;
  int i;

  // We test for offloading
#pragma omp target map(from: isOffloading)
  {
    isOffloading = !omp_is_initial_device();
  }
  
  OMPVV_WARNING_IF(!isOffloading, "With offloading off, it is not possible to test if");

  // a, init_num_threads_dev and init_num_threads_host arrays initialization
  for (i = 0; i < SIZE_N; i++) {
    a[i] = 1;
    init_num_threads_dev[i] = 0;
    init_num_threads_host[i] = 0;
  }

  // Get the init_num_threads for host and device. If it is 1, then we 
  // cannot test the if parallel 
  // See section 2.5.1 of the ref manual
#pragma omp target teams distribute parallel for num_threads(10)
  for (i = 0; i < SIZE_N; i++) {
    init_num_threads_dev[i] = omp_get_num_threads();
  }

#pragma omp parallel for num_threads(10)
  for (i = 0; i < SIZE_N; i++) {
    init_num_threads_host[i] = omp_get_num_threads();
  }
  
  // We check that none of the values is 1 or less. This would make the test of if target
  // not possible
  int raiseWarningDevice = 0, raiseWarningHost = 0;
  for (i = 0; i < SIZE_N; i++) {
    if (init_num_threads_dev[i] <= 1 ) {
      raiseWarningDevice = 1;
    }
    if (init_num_threads_host[i] <= 1) {
      raiseWarningHost = 1;
    }
  }
  OMPVV_WARNING_IF(raiseWarningDevice, "Initial number of threads in device was 1. It is not possible to test the if for parallel");
  OMPVV_WARNING_IF(raiseWarningHost, "Initial number of threads in host was 1. It is not possible to test the if for parallel");

  // We tests multiple times. Each one we check if we are running on the initial device or not
  // if we are not we increase the value by 1, and we check if the num_threads is different
  // to the one meassured before. If we are running on the host, the if should also affect the 
  // parallel clause and we should have the number of threads = 1
  for (attempt = 0; attempt < NUM_ATTEMPTS; ++attempt) {
#pragma omp target teams distribute parallel for if(parallel: attempt >= ATTEMPT_THRESHOLD)\
    map(tofrom: a) num_threads(10)
    for (i = 0; i < SIZE_N; i++) {
      if (omp_is_initial_device()) {
        a[i] += 10; // This +10 should not happen
      } else {
        a[i] += (omp_get_num_threads() == 1) ? 1 : 0; 
      }
    }
  }

  // a and b array initialization
  for (i = 0; i < SIZE_N; i++) {
    OMPVV_TEST_AND_SET(errors, a[i] != 1 + (NUM_ATTEMPTS - ATTEMPT_THRESHOLD));
  }

  OMPVV_ERROR_IF(errors, "Unexpected value after if(parallel: ...) clause");

  return errors;
}

// Test for OpenMP 4.5 target enter data with if
int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_if_no_modifier());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_if_target_modifier());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_teams_distribute_if_parallel_modifier());

  OMPVV_REPORT_AND_RETURN(errors);
}
