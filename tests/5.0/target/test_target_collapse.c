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

#define N 100

int Runtst(int gpu) {
  omp_set_num_threads(N*N);
  int A[N], B[N], C[N] = {0};
  int errors = 0;
  for (int i = 0; i < N; ++i) {
    A[i] = 0;
    B[i] = i;
  }
  int ThrdTrack[N] = {0};
#pragma omp target data map(tofrom: A, B, C, ThrdTrack) device(gpu)
#pragma omp target parallel for collapse(2) shared(A, B, C, ThrdTrack) device(gpu)
  for (int i = 0; i < N; ++i) {
    for (int j = 0; j < N; ++j) {
      int ThrdId = omp_get_thread_num();
      ThrdTrack[ThrdId] = ThrdId;
#pragma omp atomic
      A[i] += B[j];
    }
  }

  int count = 0;
  // verifying the result
  for (int i = 0; i < N; ++i) {
    if (A[i] != N * (N - 1)/2) {
      errors++;
    }
    if (ThrdTrack[i] != i) {
      errors++;
    }
  }
  return errors;
}

int main() {

  int TotGpus = omp_get_num_devices();
  int errors = 0;
  for (int gpu = 0; gpu < TotGpus; ++gpu) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, Runtst(gpu) != 0);
  }
  OMPVV_REPORT_AND_RETURN(errors);
} 
