//===--- test_declare_target_local.c --------------------===//
//
// OpenMP API Version 6.0
// Tests that the local clause has been added to the declare
// target directive ensuring proper behavior
//
//===----------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

int local_var;
#pragma omp declare target local(local_var)

int test_local() {
    int errors = 0;
    int device_value = 0;
    local_var = 5;
    #pragma omp target map(alloc: local_var)
    {
        local_var = 10;

    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, local_var != 5);
    return errors;
}

int main() {
    OMPVV_TEST_OFFLOADING;

    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, test_local() != 0);
    OMPVV_REPORT_AND_RETURN(errors);
}

