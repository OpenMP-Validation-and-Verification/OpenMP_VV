//===------ test_declare_target_nested.c ----------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
//
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int inner_fn(int a) {
  return 1 + a;
}

int outer_fn(int a) {
  return 1 + inner_fn(a);
}

int test_declared_functions() {
  int result = 0, errors = 0;

#pragma omp target map(tofrom: result)
  {
    result = outer_fn(result);
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != 2)

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

#pragma omp declare target to(outer_fn)

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_declared_functions() != 0);

  OMPVV_REPORT_AND_RETURN(errors);
}
