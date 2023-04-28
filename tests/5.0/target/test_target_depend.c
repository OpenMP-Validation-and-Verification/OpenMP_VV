//===------ test_target_depend.c --------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + depend clause.
// Tested multiple and multi-level dependency scenario
//
//===------------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define ELMTS 1024*1024

void Runtst(int gpu) {
  int errors = 0;
  int *A1 = malloc(sizeof(int) * ELMTS);
  int *A2 = malloc(sizeof(int) * ELMTS);
  int *A3 = malloc(sizeof(int) * ELMTS);
  int *A4 = malloc(sizeof(int) * ELMTS);
  int *A5 = malloc(sizeof(int) * ELMTS);
  int *A6 = malloc(sizeof(int) * ELMTS);
  int *A7 = malloc(sizeof(int) * ELMTS);

#pragma omp target data map(tofrom: A1[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for depend(out:A1[0:ELMTS]) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A1[i] = 1;
    }
  }
#pragma omp target data map(tofrom: A2[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for depend(out:A2[0:ELMTS]) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A2[i] = 2;
    }
  }
#pragma omp target data map(tofrom: A3[0:ELMTS], A1[0:ELMTS], A2[0:ELMTS]) \
        device(gpu)
  {
#pragma omp target parallel for depend(inout:A3[0:ELMTS]) \
        depend(in:A1[0:ELMTS], A2[0:ELMTS]) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A3[i] = 3;
      A3[i] = A1[i] + A2[i] + A3[i];
    }
  }
#pragma omp target data map(tofrom: A4[0:ELMTS], A3[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for depend(inout:A4[0:ELMTS]) \
        depend(in:A3[0:ELMTS]) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A4[i] = 4;
      A4[i] = A4[i] + A3[i];
    }
  }
#pragma omp target data map(tofrom: A5[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for depend(out:A5[0:ELMTS]) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A5[i] = 5;
    }
  }
#pragma omp target data map(tofrom: A6[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for depend(out:A6[0:ELMTS]) device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A6[i] = 6;
    }
  }
#pragma omp target data \
        map(tofrom: A7[0:ELMTS], A5[0:ELMTS], A6[0:ELMTS], A4[0:ELMTS]) device(gpu)
  {
#pragma omp target parallel for depend(inout:A7[0:ELMTS]) \
        depend(in:A4[0:ELMTS]) depend(in:A5[0:ELMTS]) depend(in:A6[0:ELMTS])\
        device(gpu)
    for (int i = 0; i < ELMTS; ++i) {
      A7[i] = 7;
      A7[i] = A4[i] + A5[i] + A6[i] + A7[i];
    }
  }
  // Verification
 
  for (int i = 0; i < ELMTS; ++i) {
    if (A7[i] != 28 /*Sum of 7 natural nums*/) {
      errors++;
    }
  }

  free(A1);
  free(A2);
  free(A3);
  free(A4);
  free(A5);
  free(A6);
  free(A7);
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
