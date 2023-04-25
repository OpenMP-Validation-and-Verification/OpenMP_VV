#include <stdio.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

#define A 1
#define B 3
#define X 1

int test_fp(){

  int n = A, errors=0;
  int exp_n = A + B;
  
  #pragma omp parallel
  #pragma omp scope firstprivate( n )
  {
      n = n + X;
      n = n + X;
      n = n + X;
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, (n != exp_n));

  OMPVV_ERROR_IF(errors, "\nn|EXPECTED:%d, RECEIVED:%d\n", exp_n, n);

  return errors;
}

int main(){
  int total_errors=0;

  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_fp() != 0);

  OMPVV_REPORT_AND_RETURN(total_errors);

  return total_errors;
}
