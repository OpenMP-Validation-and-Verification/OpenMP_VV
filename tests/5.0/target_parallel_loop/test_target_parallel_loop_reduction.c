//===-------------------test_target_parallel_loop_reduction.c-----------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description - This is a test program to demonstrate how reduction
// min/max/+ clauses are used with target parallel loop construct. 
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int testMin() {
  OMPVV_INFOMSG("testMin");
  int a[N];
  int b[N];
  int errors = 0;
  srand(1);

  for (int i = 0; i < N; ++i) {
    a[i] = (int) rand() / (double)(RAND_MAX / 100);
    b[i] = (int) rand() / (double)(RAND_MAX / 100);
  }

  int result = a[0] + b[0];

#pragma omp target parallel loop reduction(min:result) map(to: a[0:N], b[0:N]) map(tofrom: result)
    for (int i = 0; i < N; ++i) {
      result = fmin(a[i] + b[i], result);
    }

  int hostMin = a[0] + b[0];
  for (int i = 0; i < N; ++i) {
    hostMin = fmin(hostMin, a[i] + b[i]);
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != hostMin);
  OMPVV_ERROR_IF(host_min != result, "Minimum value on device is %d but expected minimum from host is %d.", result, hostMin);
  return errors;
}

int testMax() {
  OMPVV_INFOMSG("testMax");

  int a[N];
  int b[N];
  int errors = 0;
  srand(1);

  for (int i = 0; i < N; ++i) {
    a[i] = (int) rand() / (double)(RAND_MAX / 100);
    b[i] = (int) rand() / (double)(RAND_MAX / 100);
  }

  int result = 0;

#pragma omp target parallel loop reduction(max:result) map(to: a[0:N], b[0:N]) map(tofrom: result)
  for (int i = 0; i < N; ++i) {
    result = fmax(a[i] + b[i], result);
  }

  int hostMax = 0;
  for (int i = 0; i < N; ++i) {
    hostMax = fmax(hostMax, a[i] + b[i]);
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != hostMax);
  OMPVV_ERROR_IF(host_Max != result, "Maximum value on device is %d but expected maximum from host is %d.", result, hostMax);

  return errors;
}

int testAddition() {
  OMPVV_INFOMSG("testAddition");

  int arr[N], total = 0;
  int errors = 0;

  for (int i = 0; i < N; i++) {
    arr[i] = i;
  }
  
#pragma omp target parallel loop reduction(+:total) map(to: arr[0:N]) map(tofrom: total)
  for (int i = 0; i < N; i++) {
    total = total + arr[i];
  }

  int sum_host = 0;
  for (int i = 0; i < N; i++) {
    sum_host = sum_host + arr[i];
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, total != sum_host);

  return errors;
}


int main() {
  int total_errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(total_errors, testMin() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, testMax() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(total_errors, testAddition() != 0);

  OMPVV_REPORT_AND_RETURN(total_errors);
}
