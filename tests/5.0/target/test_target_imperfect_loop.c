//===--- test_target_imperfect_loop.c --- target map with imperfect loops ---------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// The test maps two arrays to the device and uses the collapse clause on the work 
// sharing loop construct enclosing two loops. According to 5.0 Spec if more than 
// one loop is associated with the worksharing-loop construct then the number of 
// times that any intervening code between any two associated loops will be executed 
// is unspecified but will be at least once per iteration of the loop enclosing the 
// intervening code and at most once per iteration of the innermost loop associated 
// with the construct.The value modified on the device(if oddloaded) and is verified 
// on the host for correctness.
// This test is a modified version of an example and provided by LLNL.   
//
////===----------------------------------------------------------------------===//

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 10
#define M 16

int test_target_imperfect_loop() {
  OMPVV_INFOMSG("test_target_imperfect_loop");

  int data1[N], data2[N][M];
  int errors = 0;


  for( int i = 0; i < N; i++){
    data1[i] = 0;
    for(int j = 0; j < M; j++){
      data2[i][j] = 0;
    }
  }


#pragma omp target map(tofrom: data1, data2)
  {
#pragma omp parallel for collapse(2)
      for( int i = 0; i < N; i++){
        data1[i] += i;
        for(int j = 0; j < M; j++){
          data2[i][j] += i + j;
        }
      }
  }

  for( int i=0;i<N;i++){
    OMPVV_TEST_AND_SET(errors,data1[i] < i);
    OMPVV_TEST_AND_SET(errors,data1[i] > i * M);
    for(int j=0;j<M;j++){
      OMPVV_TEST_AND_SET(errors,data2[i][j] != (i+j));
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
