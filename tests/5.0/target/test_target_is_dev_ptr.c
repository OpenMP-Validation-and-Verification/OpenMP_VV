//===------test_target_is_dev_ptr.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + is_device_ptr + device clauses
//
//===------------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define N 1024

int Runtst(int gpu) {
  int *Hst_A = malloc(sizeof(int) * N);
  int *Dev_B = (int*)omp_target_alloc(sizeof(int) * THREADS, gpu);
  int errors = 0;
  for (int i = 0; i < N; ++i) {
    Hst_A[i] = i;
  }

#pragma omp target data map(tofrom: Hst_A[0:THREADS]) device(gpu)
  {
#pragma omp target parallel for  is_device_ptr(Dev_B) device(gpu)
    for (int i = 0; i < THREADS; ++i) {
      Dev_B[i] = Hst_A[i] * Hst_A[i];
      Hst_A[i] = Dev_B[i];
    }
  }

  // Verification
 
  for (int i = 0; i < THREADS; ++i) {
    if (Hst_A[i] != i*i) {
      errors++;
    }
  }

  free(Hst_A);
  omp_target_free(Dev_B, gpu);
  return errors;
}


int main() {
  OMPVV_TEST_OFFLOADING;
  int TotGpus = omp_get_num_devices();
  int errors = 0;
  for (int gpu = 0; gpu <= TotGpus; ++gpu) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (Runtst(gpu) != 0));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
