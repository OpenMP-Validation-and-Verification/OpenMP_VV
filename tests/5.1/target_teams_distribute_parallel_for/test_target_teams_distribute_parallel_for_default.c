//-----------------test_target_teams_distribute_parallel_for_default.c-----
//
// OpenMP API Version 5.1 Mar 2023
//
// This test intends to test the functionality of an indirect
// declare target region. A random int is chosen that correlates
// to a function pointer. That function is then chosen and the
// test ensures it runs as expected.
// --------------------------------------------------------------



#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"


int DefaultFirstPrivate() {
  int ErrCount = 0;
  int Arr[32];
  for (int i = 0; i < 32; ++i) {
    Arr[i] = i;
  }

#pragma omp target teams distribute parallel for num_teams(2) thread_limit(10)\
        default(firstprivate) map(tofrom: ErrCount)
  for (int i = 0; i < 32; ++i) {
    if (Arr[i] != i) {
#pragma omp atomic
      ErrCount += 1;
    }
  }
  return ErrCount;
}

int DefaultPrivate() {
  int ErrCount = 0;
  int CONST = 123;
  int Arr[32];
  for (int i = 0; i < 32; ++i) {
    Arr[i] = i;
  }

#pragma omp target teams distribute parallel for num_teams(2) thread_limit(10)\
        default(private)
  for (int i = 0; i < 32; ++i) {
    CONST = 10;
    Arr[i] += CONST;
    if (Arr[i] != (i + 10)) {
#pragma omp atomic
      ErrCount += 1;
    }
  }
  return ErrCount;
}


int DefaultShared() {
  int ErrCount = 0;
  int CONST = 123;
  int Arr[32];
  for (int i = 0; i < 32; ++i) {
    Arr[i] = i;
  }

#pragma omp target teams distribute parallel for num_teams(2) thread_limit(10)\
        default(shared) map(tofrom: ErrCount)
  for (int i = 0; i < 32; ++i) {
#pragma omp atomic
    CONST += 10;
#pragma omp atomic
    Arr[i] += CONST;
    if (Arr[i] != (i + 10 + 123)) {
#pragma omp atomic
      ErrCount += 1;
    }
  }
  return ErrCount;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, DefaultFirstPrivate() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(errors, DefaultPrivate() != 0);
  OMPVV_TEST_AND_SET_VERBOSE(errors, DefaultShared() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
