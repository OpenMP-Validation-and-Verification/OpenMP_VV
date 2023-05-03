//===------test_target_teams_if.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// testTargetTeamsIf()
// This is a basic test to demonstrate the use if(false) and if(true)
// clause with target teams construct.
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

/**
  This is a basic test to demonstrate the use if(false) and if(true)
  clause with target teams construct.
*/
int testTargetTeamsIf(int isTrue) {
  int errors = 0;
  int isHost = 0;
  // Execute on target
  // if isTrue != 0 (true), then target region is executed by device
  // if isTrue = 0 (false), then target region is executed by host
#pragma omp target teams map(tofrom: isHost) if(isTrue)
  {
    isHost = omp_is_initial_device();
  }
  // Validate
  if (isTrue) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (isHost != 0));
  } else {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (isHost == 0));
  }
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsIf(0));
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsIf(1));
  OMPVV_REPORT_AND_RETURN(errors);
}
