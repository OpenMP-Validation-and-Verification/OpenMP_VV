//===------test_target_reduction.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + reduction + device clauses
//
//===------------------------------------------------------------------------===//
#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define THREADS 1024*32

int Runtst(int gpu) {
  int *A = malloc(sizeof(int) * THREADS);
  int *B = malloc(sizeof(int) * THREADS);
  int errors = 0;
  for (int i = 0; i < THREADS; ++i) {
    A[i] = i;
    B[i] = i;
  }
  int TotSum = 0;
#pragma omp target data map(tofrom: A[0:THREADS], B[0:THREADS], TotSum) device(gpu)
  {
#pragma omp target parallel for reduction(+:TotSum) device(gpu)
    for (int i = 0; i < THREADS; ++i) {
      TotSum = TotSum + A[i] + B[i];
    }
  }

  if (TotSum != (THREADS - 1) * (THREADS)) {
    errors++;
  }
  free(A);
  free(B);
  return errors;
}


int main() {
  int TotGpus = omp_get_num_devices();
  int errors = 0;
  for (int gpu = 0; gpu < TotGpus; ++gpu) {
    errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, (Runtst(gpu) != 0));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
