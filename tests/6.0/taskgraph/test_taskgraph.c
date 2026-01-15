//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraph():
// Create a taskgraph
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraph(void)
{
    #pragma omp parallel shared(x)
    {
        # pragma omp single
        {
            # pragma omp taskgraph
            {
            }
        }
    }
    return 0;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraph());
    OMPVV_REPORT_AND_RETURN(errors);
}
