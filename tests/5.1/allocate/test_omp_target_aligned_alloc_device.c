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
#include <stdint.h>
#include "ompvv.h"

#define N 1024

int test_omp_aligned_alloc_on_host() {
  int errors = 0;
  int *x, *y;

  omp_memspace_handle_t  memspace = omp_default_mem_space;
  omp_alloctrait_t       traits[1] = {{omp_atk_alignment, 64}};
  omp_allocator_handle_t alloc = omp_init_allocator(memspace,1,traits);

  x = (int *)omp_aligned_alloc(64, N*sizeof(int), alloc);
  y = (int *)omp_aligned_alloc(64, N*sizeof(int), alloc);
    
  OMPVV_TEST_AND_SET_VERBOSE(errors, ((intptr_t)(y))%64 != 0 || ((intptr_t)(x))%64 != 0);

  #pragma omp parallel for simd simdlen(16) aligned(x,y: 64)
  for (int i = 0; i < N; i++) {
    x[i] = i;
    y[i] = i+1;
  }

  #pragma omp parallel for simd simdlen(16) aligned(x,y: 64)
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, x[i] != i);
    OMPVV_TEST_AND_SET_VERBOSE(errors, y[i] != i+1);
  }

  omp_free(x, alloc);
  omp_free(y, alloc);
  omp_destroy_allocator(alloc);

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_omp_aligned_alloc_on_host() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}


