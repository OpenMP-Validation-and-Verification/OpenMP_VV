//===------test_target_teams_device.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// testTargetTeamsDeviceMultDevFunc()
// This is a basic test to demonstrate
// clause with target teams construct.
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

/**
  This is a basic test to demonstrate 
  clause with target teams construct.
*/

int testTargetTeamsDeviceMultDevFunc() {
  int errors = 0, numOfDev = 0;
  numOfDev = omp_get_num_devices();
  int *A = (int *) malloc(N*numOfDev*sizeof(int));
  int *B = (int *) malloc(N*numOfDev*sizeof(int));
  int *C = (int *) malloc(N*numOfDev*sizeof(int));
  if ((A == NULL) || (B == NULL) || (C == NULL)) {
    return 1;
  }
  // Data Inititalize
  for (int i = 0; i < (N*numOfDev); i++) {
    A[i] = 2*i;      // Even
    B[i] = 2*i + 1;  // Odd
    C[i] = 0;
  }
  for (int dev = 0; dev < numOfDev; dev++) {
    int idx_from = dev*N, idx_to = (dev + 1)*N;
#pragma omp target data map(to: A[idx_from:idx_to]) map(to: B[idx_from:idx_to]) \
        map(tofrom: C[idx_from:idx_to]) device(dev)
    {
#pragma omp target teams map(to: A[idx_from:idx_to]) map(to: B[idx_from:idx_to]) \
        map(tofrom: C[idx_from:idx_to]) device(dev)
      {
        for (int i = idx_from; i < idx_to; i++) {
          C[i] = A[i] + B[i];
        }
      }
    }
  }
  // Validate
  for (int i = 0; i < (N*numOfDev); i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, C[i] != A[i] + B[i]);
  }
  free(C);
  free(B);
  free(A);
  return errors;
}

int testTargetTeamsDeviceDefault() {
  int errors = 0, numOfDev = 0;
  numOfDev = omp_get_num_devices();
  int *device = (int *) malloc(numOfDev*sizeof(int));
  if (device == NULL) {
    return 1;
  }
  for (int dev = 0; dev < numOfDev; dev++) {
#pragma omp target teams map(tofrom: device[dev]) device(dev)
    {
      device[dev] = omp_get_device_num();
    }
  }
  for (int dev = 0; dev < numOfDev; dev++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (device[dev] != dev));
  }
  free(device);
  // Validate
  return errors;
}

int testTargetTeamsDeviceWithMod() {
  int errors = 0, numOfDev = 0;
  numOfDev = omp_get_num_devices();
  int *device = (int *) malloc(numOfDev*sizeof(int));
  if (device == NULL) {
    return 1;
  }
  for (int dev = 0; dev < numOfDev; dev++) {
#pragma omp target teams map(tofrom: device[dev]) device(device_num:dev)
    {
      device[dev] = omp_get_device_num();
    }
  }
  for (int dev = 0; dev < numOfDev; dev++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (device[dev] != dev));
  }
  free(device);
  // Validate
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsDeviceDefault());
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsDeviceWithMod());
  OMPVV_TEST_AND_SET_VERBOSE(errors, testTargetTeamsDeviceMultDevFunc());
  OMPVV_REPORT_AND_RETURN(errors);
}
