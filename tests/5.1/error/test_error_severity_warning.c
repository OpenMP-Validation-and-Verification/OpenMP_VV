//===--- test_error_severity_warning.c ----------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  Test uses error directive with message clause to print a message,
//  if error condition is reached then "runtime error!" is printed,
//  otherwise is simply used to print a message. 
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int errors, i;

int test_error_severity_warning() {
    i = 0;
    printf("Test should print a 'warning' error message")
    #pragma omp error severity(warning)
    i+=5;
    OMPVV_WARNING_IF("Error directive caused runtime error", i!=5);
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_error_severity_warning() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}
