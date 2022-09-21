//===--------------------- test_interop_target.c ----------------------------===//
//
// OpenMP API Version 5.1 Nov 2020
// 
// Test for interop construct utilizing target as the interopability. Runs
// a OpenMP target construct and treating it as an external operation to test
// if the interop contrruct properly executes and returns the result. Test
// passes if the results are processed correctly and fails if not.
//
////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int interopTestTarget() {
  int errors = 0;
  int A[N];

  for (int i = 0; i < N; i++) {
    A[i] = 0
  }


  //OMPVV_TEST_AND_SET(errors, condition)
}

int main () {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;

  int numdevices = omp_get_num_devices();

  OMPVV_WARNING_IF(numdevices <= 0, "No devices detected, interop target test running on host");

  errors = interopTestTarget();

  OMPVV_REPORT_AND_RETURN(errors);:

  return 0;
}
