//===------ test_target_if_device.c --------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + if + device clauses
//
//===------------------------------------------------------------------------===//
#include <stdio.h>
#include "omp.h"
#include "ompvv.h"
#define N 1024



int Runtst(int gpu, int Proc) { // Proc = 0(host), 1(gpu)
  int *A = malloc(sizeof(int) * N), Errors = 0;
  int *InitDev = malloc(sizeof(int));
  for (int i = 0; i < N; ++i) {
    A[i] = i;
  }

#pragma omp target data map(tofrom: A[0:N], InitDev[0:1]) if(Proc) device(gpu)
  {
#pragma omp target parallel for if(Proc) device(gpu)
    for (int i = 0; i < N; ++i) {
      if (i == 0) {
        InitDev[0] = omp_is_initial_device();
      }
      A[i] = A[i] * A[i];
    }
  }

  // Verification
  for (int i = 0; i < N; ++i) {
    if (A[i] != i*i) {
      Errors++;
    }
  }
  // Verifying if target block ran on intended processor
  if (InitDev[0] != (!Proc)) {
    Errors++;
  }

  free(A);
  return Errors;
}


int main() {
  OMPVV_TEST_OFFLOADING;
  int TotGpus = omp_get_num_devices();
  int errors = 0;
  for (int gpu = 0; gpu < TotGpus; ++gpu) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, (Runtst(gpu, 0 /*for cpu*/) != 0));
    OMPVV_TEST_AND_SET_VERBOSE(errors, (Runtst(gpu, gpu < TotGpus /*for gpu*/) != 0));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
