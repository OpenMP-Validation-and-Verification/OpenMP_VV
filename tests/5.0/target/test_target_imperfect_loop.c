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

#define N 10

int test_target_imperfect_loop() {
  OMPVV_INFOMSG("test_target_imperfect_loop");

  int data1[N], data2[N][N];
  int errors = 0;


  for( int i = 0; i < N; i++){
    data1[i] = 0;
    for(int j = 0; j < N; j++){
      data2[i][j] = 0;
    }
  }


#pragma omp target map(tofrom: data1, data2)
  {
#pragma omp parallel for collapse(2)
      for( int i = 0; i < N; i++){
        data1[i] += i;
        for(int j = 0; j < N; j++){
          data2[i][j] += i + j;
        }
      }
  }

  for( int i=0;i<N;i++){
    OMPVV_TEST_AND_SET(errors,data1[i] != i);
    printf("data1[%d] = %d \n", i, data1[i]);
    for(int j=0;j<N;j++){
      OMPVV_TEST_AND_SET(errors,data2[i][j] != (i+j));
 //     printf("data2[%d][%d] = %d \n", i, j, data2[i][j]);
    }
  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_imperfect_loop());

  OMPVV_REPORT_AND_RETURN(errors);
}
