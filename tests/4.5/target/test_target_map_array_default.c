//===--test_target_map_array_default.c - test default behavior of array map--===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// Whenever a map-type-modifier is not specified in the map clause, the symbol
// is mapped as a tofrom. This test make sure this is satisfied
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 1000

int main() {
  int compute_array[N];
  int sum = 0, result = 0, errors = 0;
  int i;
  
  OMPVV_TEST_OFFLOADING;

  // Array initialization
  for (i=0; i<N; i++) 
    compute_array[i] = 10;

#pragma omp target map(compute_array)
  {
    for (i = 0; i < N; i++)
      compute_array[i] += i;
   
  } // End target

  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];
  
  for (i = 0; i < N; i++)
    result += 10 + i;

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != sum);

  OMPVV_REPORT_AND_RETURN(errors)
}
