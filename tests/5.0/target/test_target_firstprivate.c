//===------ test_target_firstprivate.c --------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + firstprivate clauses
//
//===------------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define ELMTS 1024*1024
#define INIT_VAL 9999


void Runtst(int gpu) {
  int *A = malloc(sizeof(int) * ELMTS), errors = 0;
  for (int i = 0; i < ELMTS; ++i) {
    A[i] = i;
  }
  int HostVar = INIT_VAL;
  omp_set_num_threads(ELMTS);
#pragma omp target data map(tofrom: A[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for firstprivate(HostVar) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      if (HostVar == INIT_VAL) {
        A[i] = A[i] * A[i];
      } else {
        A[i] = 0;
      }
    }
  }

  // Verification
  // Also validate for gpu ordinal 
  for (int i = 0; i < ELMTS; ++i) {
    if (A[i] != i*i) {
      errors++;
    }
  }

  free(A);
  return errors;
}


int main() {
  int TotGpus = omp_get_num_devices();
  int errors = 0;
  for (int gpu = 0; gpu < TotGpus; ++gpu) {
   OMPVV_TEST_AND_SET_VERBOSE(errors, (Runtst(gpu) != 0));
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
