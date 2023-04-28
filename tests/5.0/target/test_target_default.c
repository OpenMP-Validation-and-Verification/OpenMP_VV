//===------ test_target_default.c --------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with parallel for + default clause.
//
//===------------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define N 100


int IfTstPassed = 1; // 1 is passed 0 is failed

int main(int argc, char** argv) {
  int count = 123, errors = 0;
#pragma omp target data map(tofrom: count)
#pragma omp target parallel for shared(count, IfTstPassed) default(none)
  for (int i = 0; i < N; ++i) {
    if (count != 123) {
      IfTstPassed = 0;
    }  
  }

  count = 123;
#pragma omp target parallel for default(shared)
  for (int i = 0; i < N; ++i) {
    if (count != 123) {
      IfTstPassed = 0;
    }
  }
  
  if (!IfTstPassed) {
    errors++;
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, IfTstPassed != 1);
  OMPVV_REPORT_AND_RETURN(errors);
}

