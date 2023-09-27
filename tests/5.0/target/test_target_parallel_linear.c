//===------test_target_linear.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + device + linear clauses
//
//===------------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define N 1024



int Runtst(int gpu) {
  int *A = (int *) malloc(sizeof(int) * N), errors = 0;
  for (int i = 0; i < N; ++i) {
    A[i] = i;
  }
  int val = 0;
#pragma omp target data map(tofrom: A[0:N], val) device(gpu)
  {
#pragma omp target parallel for device(gpu) linear(val:2)
    for (int i = 0; i < N; ++i) {
      val += 2;
      A[i] = val;
    }
  }

  // Verification
 
  for (int i = 0; i < N; ++i) {
    if (A[i] != (i+1) * 2) {
      errors++;
    }
  }

  free(A);
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
