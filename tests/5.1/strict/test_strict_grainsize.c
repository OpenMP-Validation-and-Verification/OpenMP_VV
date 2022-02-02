//===--- test_strict_grainsize.c -----------------------------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test checks behavior of the default clause when the specified data-sharing-attribute  
//  is firstprivate. The constructs allowed for a default clause are parallel, teams, task, and taskloop.
//  This test utilizes the taskloop construct, where firstprivate states that one or more list items are
//  private to the parallel task, and each of them are initialized with the value of the corresponding
//  original item. The taskloop construct executes one or more loops in parallel on each thread. 
//
////===--------------------------------------------------------------------------------------===//
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors;

int test_strict_grainsize() {
  int scalar_var = 6;
  int arr[N];
  int newarr[N];
  int sum =0;
  for (int i=0; i<N; i++){
        arr[i] = i;
        sum += arr[i];
 }
#pragma omp parallel shared(arr, newarr)
  {   
#pragma omp taskloop grainsize(1000)
  for (int i = 0; i < N; i++) {
                newarr[i] = arr[i] + arr[i];
                scalar_var += 7;
        }
  }
  int new_sum =0;
  for (int i = 0; i < N; i++) {
        new_sum += newarr[i];
  }
  OMPVV_TEST_AND_SET(errors, sum == new_sum);
  OMPVV_INFOMSG_IF(new_sum == 0, "Array was not initialzed.");
  OMPVV_INFOMSG_IF(new_sum==sum, "Something went wrong.");
  return errors;
}

int main() {
   errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_strict_grainsize() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}

