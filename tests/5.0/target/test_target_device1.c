//===------test_target_device.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// This is a basic test to demonstrate target + parallel for + device clause
//
//===------------------------------------------------------------------------===//
#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define THREADS 1024*1024*1024


int Runtst(int gpu) {
  int errors = 0;
  int *A = malloc(sizeof(int) * THREADS);
  for (int i = 0; i < THREADS; ++i) {
    A[i] = i;
  }

#pragma omp target data map(tofrom: A[0:THREADS]) device(gpu)
  {
    #pragma omp target parallel for device(gpu)
    for (int i = 0; i < THREADS; ++i) {
      A[i] = A[i] * A[i];
    }
  }

  // Verification
 
  for (int i = 0; i < THREADS; ++i) {
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
