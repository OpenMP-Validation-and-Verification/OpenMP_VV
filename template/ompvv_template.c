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
  int errors = 1;

#pragma omp target map(from: errors)
  {
    errors = 0;
  }

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING; // This should be at the beginning of all tests

  // Here are some examples on how to use the VERBOSE_MODE messages
  OMPVV_INFOMSG("THIS IS IS AN EXAMPLE OF AN INFOMSG");
  OMPVV_INFOMSG("THIS IS IS AN EXAMPLE OF AN INFOMSG WITH PARAMETERS (%d, %s, %f)", 3, "ompvv", 3.3);
  OMPVV_WARNING("THIS IS IS AN EXAMPLE OF A WARNING");
  OMPVV_WARNING("THIS IS IS AN EXAMPLE OF A WARNING WITH PARAMETERS (%d, %s, %f)", 3, "ompvv", 3.3);
  OMPVV_ERROR("THIS IS IS AN EXAMPLE OF AN ERROR");
  OMPVV_ERROR("THIS IS IS AN EXAMPLE OF AN ERROR WITH PARAMETERS (%d, %s, %f)", 3, "ompvv", 3.3);


  int errors = 0;
  OMPVV_TEST_AND_SET(errors, 1==0);
  errors = 0;
  errors = test_function();
  // CALL OTHER FUNCTIONS HERE

  // These two lines could be replaced with OMPVV_REPORT_AND_RETURN(errors)
  // OMPVV_REPORT requires OMPVV_TEST_OFFLOADING TO BE CALLED BEFOREHAND
  OMPVV_REPORT(errors);   
  OMPVV_RETURN(errors); 
}
