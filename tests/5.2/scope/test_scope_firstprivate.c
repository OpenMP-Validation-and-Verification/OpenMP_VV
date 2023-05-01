//===--- test_scope_firstprivate.c -----------------------------------------===//
//
// OpenMP API Version 5.2 Nov 2021 
//
//This test ensures that according to the first private clause, all
//threads in a team keep private copies of
//a given integer variable instead of simultaniously updating the same
//reference. The scope of the team is determined by the scope directive,
//which applies the given firstprivate clause to the innermost parallel region.
//===-----------------------------------------------------------------------===//
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

#define A 1
#define B 2
#define X 1

int test_fp(){

  int n = A, errors=0;
  int exp_n = B;
  
  #pragma omp parallel
  #pragma omp scope firstprivate( n )
  {
      n = n + X;
      n = n + X;
      n = n + X;
  
      OMPVV_TEST_AND_SET_VERBOSE(errors, (n != exp_n));
      OMPVV_ERROR_IF(errors, "\nn|EXPECTED:%d, RECEIVED:%d\n", exp_n, n);
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, (n != A));
  OMPVV_ERROR_IF(errors, "\nn|EXPECTED:%d, RECEIVED:%d\n", A, n);

  return errors;
}

int main(){
  int total_errors=0;

  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_fp() != 0);

  OMPVV_REPORT_AND_RETURN(total_errors);

  return total_errors;
}
