//-------------test_scope_construct.c---------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// Tests the behavior of the scope construct with no clauses
// specified.
// Offloads to a device.
//--------------------------------------------------------//

#include "ompvv.h"
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 64

int test_scope() {
  int errors = 0;
  int total = 0;
  #pragma omp target parallel shared(total) map(tofrom : total)
  {
    #pragma omp scope
    {
      #pragma omp for
      for (int i = 0; i < N; ++i) {
        #pragma omp atomic update
        ++total;
      }
    }
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, total != N);
  return errors;
}
int main() {
  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_scope() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
