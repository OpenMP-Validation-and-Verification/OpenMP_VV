//===--- test_error_at_runtime.c ----------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  Test uses error directive with at(error) clause to print a message,
//  if error condition is reached during runtime, where it will print a,
//  error message. If error message does not print, then the test has failed. 
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int errors, i;

int test_error_at_runtime() {
    i = 0;
    printf("Test should print a 'runtime' error message: \n");
    #pragma omp parallel
    {
        #pragma omp error at(runtime)
        i+=5;
    }
    OMPVV_WARNING_IF("Error directive caused runtime error", i!=5);
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_error_at_runtime() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}
