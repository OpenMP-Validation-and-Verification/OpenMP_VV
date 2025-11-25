//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraph():
// Create a taskgraph
// Ensures the structured block is executed
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraph(void)
{
    int errors = 0;
    int x = 0;
    #pragma omp parallel shared(x)
    {
        # pragma omp single
        {
            # pragma omp taskgraph
            {
                ++x;
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 1);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraph());
    OMPVV_REPORT_AND_RETURN(errors);
}
