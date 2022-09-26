//===--- test_depend_inoutset.c ----------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test verifies the use of inoutset in depend clause.
//  Task T4 & T5 both rely on the use of c, and should run in any order.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors;
int arr[N];
int depend_inoutset(){
  #pragma omp parallel
  #pragma omp single
  {
    for(int i = 0; i < N; i++){
      #pragma omp task depend(out: arr[i])
      arr[i] = i + 1;
    }
    for(int i = 0; i < N; i++){
      #pragma omp task depend(inoutset: arr[i])
      arr[i] = arr[i] + 2;
    }
    for(int i = 0; i < N; i++){
      #pragma omp task depend(inoutset: arr[i])
      arr[i] = arr[i] + 3;
    }
  }
  for(int i = 0; i < N; i++){
    OMPVV_TEST_AND_SET_VERBOSE(errors, arr[i] != i + 6);
  }
  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, depend_inoutset() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}