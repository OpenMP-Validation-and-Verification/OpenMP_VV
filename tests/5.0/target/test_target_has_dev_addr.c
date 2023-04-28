//===------ test_target_has_dev_addr.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + device + has_device_addr clauses
//
//===------------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define THREADS 1024*1024*1024

void Runtst(int gpu) {
  int *Hst_A = malloc(sizeof(int) * THREADS);
  int *Hst_B = malloc(sizeof(int) * THREADS);
  int errors = 0;
  for (int i = 0; i < THREADS; ++i) {
    Hst_A[i] = i;
    Hst_B[i] = 2*i;
  }

#pragma omp target enter data map(to: Hst_B[0:THREADS])

#pragma omp target data map(tofrom: Hst_A[0:THREADS]) device(gpu)
  {
#pragma omp target parallel for  device(gpu) has_device_addr(Hst_B)
    for (int i = 0; i < THREADS; ++i) {
      Hst_B[i] = Hst_A[i] * Hst_B[i];
    }
  }

  // Verification
 
  for (int i = 0; i < THREADS; ++i) {
    if (Hst_B[i] != 2*i*i) {
      errors++;
    }
  }

  free(Hst_A);
#pragma omp target exit data map(release: Hst_B[0:THREADS])
  free(Hst_B);
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
