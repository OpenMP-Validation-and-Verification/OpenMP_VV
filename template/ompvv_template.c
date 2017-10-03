// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===------ FILE_NAME.c ------------------ Test title ---------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// 
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

int test_function() {
  int errors = 0;
  //
  // WRITE YOUR TEST HERE
  //

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  errors = test_function();
  // CALL OTHER FUNCTIONS HERE

  // These two lines could be replaced with OMPVV_REPORT_AND_RETURN(errors)
  // OMPVV_REPORT requires OMPVV_TEST_OFFLOADING TO BE CALLED BEFOREHAND
  OMPVV_REPORT(errors);   
  OMPVV_RETURN(errors); 
}
