//===------ test_omp_aligned_alloc.c ------------------------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// Tests the omp_aligned_alloc routine properly requests a memory allocation
// aligned to the passed byte parameter. Starts by initializing some memory
// in the omp_default_mem_space and then checks if this memory is aligned
// and can be properly written to and read from.
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_omp_aligned_alloc() {
  int errors = 0;
  float *x, *y;


  #pragma omp target map(from:x[:N])
  {
    #pragma omp parallel for simd simdlen(16) aligned(x: 64)
    for (int i = 0; i < N; i++) {
      x[i] = i;
    }
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != i);
  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_omp_aligned_alloc() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}


