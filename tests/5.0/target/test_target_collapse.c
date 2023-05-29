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
#define ITERS 5
int Runtst(int gpu) {
  omp_set_num_threads(N);
  int A[N], B[N] = {0};
  int errors = 0;
  for (int i = 0; i < N; ++i) {
    A[i] = 0;
    B[i] = i + 1;
  }
  int ThrdTrack[ITERS * ITERS + ITERS] = {0}; // an array of 30 elements
#pragma omp target data map(tofrom: A, B, ThrdTrack) device(gpu)
#pragma omp target parallel for collapse(2) shared(A, B, ThrdTrack) device(gpu)
  for (int i = 0; i < ITERS; ++i) {
    for (int j = 0; j < ITERS; ++j) {
      int ThrdId = omp_get_thread_num();
      ThrdTrack[ThrdId] = ThrdId;
#pragma omp atomic
      A[i] += B[j];
    }
  }

  int count = 0;
  // verifying the result
  for (int i = 0; i < N; ++i) {
    if (A[i] != N * (N + 1)/2) {
      errors++;
    }
    if (ThrdTrack[i] != i) {
      errors++;
    }
  }
  // since only 25 threads are expected to get launched the remaining 5 places
  // of ThrdTrack[] array should still have zeros
  for (int i = 25; i < 30; ++i) {
    if (ThrdTrack[i] != 0) {
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
