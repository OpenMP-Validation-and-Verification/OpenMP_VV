//===---- test_omp_in_explicit_task.c -----------------------------------------===//
// 
// OpenMP API Version 5.1
//
// Uses omp_in_explicit_task() runtime call to ensure it is 1 when in an explicit
// task and 0 otherwise.
// 
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ompvv.h"
#include <stdbool.h>

#define N 1024

int errors;

int test_wrapper() { 
    errors = 0;
    int A[N];
    for(int i = 0; i < N; i++){
        A[i] = 0;
    }
    #pragma omp task shared(A)// creates EXPLICIT task, omp_in_explicit_task = 1
    {
        for(int i = 0; i < N; i++){
            A[i] = omp_in_explicit_task();
        }
    }
    for(int i = 0; i < N; i++){
        #pragma omp parallel for // creates IMPLICIT tasks, omp_in_explicit_task = 0
        for(int i = 0; i < N; i++){
            A[i] += omp_in_explicit_task();
        }
    }
    for(int i = 0; i < N; i++){
        OMPVV_TEST_AND_SET(errors, A[i] != 1);
    }
    OMPVV_WARNING_IF(A[5] == 2, "Parallel construct incorrectly recognized as explicit task");
    OMPVV_WARNING_IF(A[5] == 0, "Task construct not recognized by omp_in_explicit_task()");
    return errors;
}

int main () {
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_wrapper());
   OMPVV_REPORT_AND_RETURN(errors);
}  
