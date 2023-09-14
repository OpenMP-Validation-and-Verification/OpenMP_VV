//===------test_target_device.c--------------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// Description:
// This is a basic test to demonstrate target + parallel for + device clause
//
//===------------------------------------------------------------------------===//
#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define N 1024


int Runtst(int gpu) {
  int errors = 0;
  int *A = (int *)malloc(sizeof(int) * N);
  for (int i = 0; i < N; ++i) {
    A[i] = i;
  }

#pragma omp target data map(tofrom: A[0:N]) device(gpu)
  {
    #pragma omp target parallel for device(gpu)
    for (int i = 0; i < N; ++i) {
      A[i] = A[i] * A[i];
    }
  }

  // Verification
 
  for (int i = 0; i < N; ++i) {
    if (A[i] != i*i) {
      errors++;
    }
  }

  free(A);
  return errors;
}


int main() {
  OMPVV_TEST_OFFLOADING;
  int TotGpus = omp_get_num_devices();
  printf("TotGpus: %d\n", TotGpus);
  int errors = 0;
  for (int gpu = 0; gpu < TotGpus; ++gpu) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (Runtst(gpu) != 0)); 
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
