//===------------------------- test_target_map_present_ordering.c ----------------------------------===//
//
// OpenMP API Version 5.1 Nov 2020
//
// This test checks tests the present map-type-modifier on a map clause. This checks to verify
// that when two map clauses are added, the one with a present modifier is processed first.
// This is a black box test, however this should compile properly and the to map should be
// handled first.
//
////===--------------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int errors; 

int test_present_map_reordering() {
  int x[N];

  for (int i = 0; i < N; i++) { 
    x[i] = i;
  }

 
  #pragma omp target map(present, to: x) map(from: x)
  {
    for (int i = 0; i < N; i++) {
      x[i] += i;
    }
  }

  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, x[i] != i*2);
  }
 
  return errors;  	 
}

int main () {
  
  errors = 0;
  
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_present_map_reordering());
  OMPVV_REPORT_AND_RETURN(errors);
}
