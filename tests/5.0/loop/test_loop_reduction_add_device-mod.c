//===--- test_loop_reduction_add_device-mod.c ------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test uses the reduction clause on a loop directive, testing that the
// variables in the reduction clause are properly reduced using the add
// operator. This test checks the above in a target context.
// Test written in reference to test_loop_reduction_add_device.c.
//
//===----------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#define A -32 
#define B -32.0 
#define L '(' 

int test_add() {
  int n = A, errors=0; 
  float f = B;
  double d = B;
  char c = L;
  int exp_n = A + 64;
  float exp_f = B + 64.0;
  double exp_d = B + 64.0;
  char exp_c = L + 64;

#pragma omp target map(tofrom:n,f,d,c) 
  {
  #pragma omp loop reduction(+:n,f,d,c) 
    for(int i=0;i<64;i++){
      n = n + 1;
      f = f + 1.0;  
      d = d + 1.0;  
      c = c + 1;
    }
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, (n != exp_n) || (f != exp_f) || (d != exp_d) || (c != exp_c));

  OMPVV_ERROR_IF(errors, "\nn|EXPECTED:%d, RECEIVED:%d\nf|EXPECTED:%.1f, RECEIVED:%.1f\nd|EXPECTED:%.1f, RECEIVED:%.1f\nc|EXPECTED:%c, RECEIVED:%c\n",exp_n,n,exp_f,f,exp_d,d,exp_c,c); 

  return errors;
}
int main() {
  OMPVV_TEST_OFFLOADING;

  int total_errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(total_errors, test_add() != 0);

  OMPVV_REPORT_AND_RETURN(total_errors);
}
