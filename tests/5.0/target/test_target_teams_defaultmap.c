//===------test_target_teams_defaultmap.c--------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Description:
// testDefaultMapToFrom()
// This is a basic test to demonstrate defaultmap(tofrom)
// clause with target teams construct.
//
// testDefaultMapFrom()
// This is a basic test to demonstrate defaultmap(from)
// clause with target teams construct.
// testDefaultMapTo()
// This is a basic test to demonstrate defaultmap(to)
// clause with target teams construct.
// testDefaultMapDefault()
// This is a basic test to demonstrate defaultmap(default)
// clause with target teams construct.
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

/**
  This is a basic test to demonstrate defaultmap(tofrom)
  clause with target teams construct.
*/
int testDefaultMapToFrom() {
  int errors = 0;
  int device_data[N], host_data[N];
  for (int i = 0; i < N; i++) {
    device_data[i] = i;
    host_data[i] = 0;
  }
#pragma omp target teams defaultmap(tofrom)
  {
    for (int i = 0; i < N; i++) {
      host_data[i] = device_data[i];
    }
  }
  // checking results
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, host_data[i] != device_data[i]);
  }
  return errors;
}
/**
  This is a basic test to demonstrate defaultmap(from)
  clause with target teams construct.
*/
int testDefaultMapFrom() {
  int errors = 0;
  int device_data[N], host_data[N];
  for (int i = 0; i < N; i++) {
    device_data[i] = i;
    host_data[i] = 0;
  }
#pragma omp target teams defaultmap(from)
  {
    for (int i = 0; i < N; i++) {
      host_data[i] = i;
    }
  }
  // checking results
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, host_data[i] != i);
  }
  return errors;
}
/**
  This is a basic test to demonstrate defaultmap(to)
  clause with target teams construct.
*/
int testDefaultMapTo() {
  int errors = 0;
  int device_data[N], host_data[N];
  for (int i = 0; i < N; i++) {
    device_data[i] = i;
    host_data[i] = 0;
  }
#pragma omp target teams defaultmap(to)
  {
    for (int i = 0; i < N; i++) {
      host_data[i] = device_data[i];
    }
  }
  // checking results
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, host_data[i] != 0);
  }
  return errors;
}
/**
  This is a basic test to demonstrate defaultmap(default)
  clause with target teams construct.
*/
int testDefaultMapDefault() {
  int errors = 0;
  int device_data[N], host_data[N];
  for (int i = 0; i < N; i++) {
    device_data[i] = i;
    host_data[i] = 0;
  }
#pragma omp target teams defaultmap(default)
  {
    for (int i = 0; i < N; i++) {
      host_data[i] = device_data[i];
    }
  }
  // checking results
  for (int i = 0; i < N; i++) {
    OMPVV_TEST_AND_SET(errors, host_data[i] != device_data[i]);
  }
  return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, testDefaultMapToFrom());
  OMPVV_TEST_AND_SET_VERBOSE(errors, testDefaultMapFrom());
  OMPVV_TEST_AND_SET_VERBOSE(errors, testDefaultMapTo());
  OMPVV_TEST_AND_SET_VERBOSE(errors, testDefaultMapDefault());
  OMPVV_REPORT_AND_RETURN(errors);
}
