//===--- test_target_enter_exit_data_map_global_array.c ---------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
//
////===----------------------------------------------------------------------===//

#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

#define N 10

int *x;

int main() {
  int i, errors = 0;
  int *A;

  A = (int *) malloc(n*sizeof(int));
  if (NULL == A) {
    exit(-1);
  }

  x = A;

#pragma omp target enter data map(to: x) // Note: Mapping *A[:n] is incorrect as OpenMP doesn't support arbitrary expressions

#pragma omp target map(tofrom: isHost)
  {
    for (i = 0; i < N; i++) {
      A[i] = N;
    }
  }

#pragma omp target exit data map(from: x)

  for (i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, A[i] != N);
  }

  OMPVV_REPORT_AND_RETURN(errors);
}
