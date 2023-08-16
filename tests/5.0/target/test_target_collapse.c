//===------ test_target_collapse.c --------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with collapse clause.
//
//===------------------------------------------------------------------------===//


#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define N 5
int Runtst(int gpu) {
  int A[N], B[N] = {0};
  int errors = 0;
  for (int i = 0; i < N; ++i) {
    A[i] = 0;
    B[i] = i + 1;
  }
#pragma omp target data map(tofrom: A, B) device(gpu)
#pragma omp target parallel for collapse(2) shared(A, B) device(gpu)
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j) {
#pragma omp atomic
      A[i] += B[j];
    }
  }

  // verifying the result
  for (int i = 0; i < N; ++i) {
    if (A[i] != N * (N + 1)/2) {
      errors++;
    }
  }
  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int TotGpus = omp_get_num_devices();
  int errors = 0;
  for (int gpu = 0; gpu < TotGpus; ++gpu) {
    OMPVV_TEST_AND_SET(errors, Runtst(gpu) != 0);
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
