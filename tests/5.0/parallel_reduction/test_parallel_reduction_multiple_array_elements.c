//===--- test_parallel_reduction_nested.c -----------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This is a simple test of the parallel directive with a reduction clause
// which performs a reduction on two different elements of the same array,
// in an offloaded region.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

static double temps[2];

int main() {
  int errors = 0;

  OMPVV_TEST_OFFLOADING;

  temps[0] = 0;
  temps[1] = 0;

#pragma omp target map(tofrom: temps)
  {
#pragma omp parallel reduction(+:temps[0], temps[1])
    {
      temps[0] += 1;
      temps[1] += 1;
    }
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, temps[0] != 1 || temps[1] != 1);

  OMPVV_REPORT_AND_RETURN(errors);
}
