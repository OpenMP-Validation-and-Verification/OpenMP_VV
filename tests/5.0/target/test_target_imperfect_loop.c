//===--- test_target_imperfect_loop.c --- target map with imperfect loops ---------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// The test maps a 2-D array to the device and uses the collapse clause on the nested 
// parallel directive to update value of the array.
// The value is verified on the host for correctness.
// This test is adapted from the examples and provided by LLNL.   
//
////===----------------------------------------------------------------------===//

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

int test_target_imperfect_loop() {
  OMPVV_INFOMSG("test_target_imperfect_loop");

  double data[N][N];


  for( int i = 0; i < N; i++){
    for(int j = 0; j < N; j++){
      data[i][j] = 0.0;
    }
  }


#pragma omp target map(tofrom: data)
  {
#pragma omp parallel for collapse(2)
    {
      for( int i = 0; i < N; i++){
        data[i][i] = 10*i;
        for(int j = 0; j < N; j++){
          data[i][j] += i + j;
        }
      }
    }
  }

  int sum = 0.0;

  for( int i=0;i<N;i++){
    for(int j=0;j<N;j++){
      sum += data[i][j] - (i+j);
    }
  }


  OMPVV_TEST_AND_SET(errors,sum!=(N*(N-1)*5));

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_imperfect_loop());

  OMPVV_REPORT_AND_RETURN(errors);
}
