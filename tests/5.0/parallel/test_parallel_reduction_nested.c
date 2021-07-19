//===--- test_parallel_reduction_nested.c -----------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This is a simple test of the parallel directive with a reduction clause
// nested within the same, and offloaded to the device. Thanks to Kelvin Li
// for providing this test.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

int main() {
  int errors = 0;
  int t = -1;

  OMPVV_TEST_OFFLOADING;

#pragma omp target teams map(tofrom: t)
  {
#pragma omp parallel reduction(+: t)
    {
#pragma omp parallel reduction(+: t)
      {
        t = 1;
      }
    }
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, t != 1);

  OMPVV_REPORT_AND_RETURN(errors);
}
