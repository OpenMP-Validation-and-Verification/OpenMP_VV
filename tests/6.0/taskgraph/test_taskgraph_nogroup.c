//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphNoGroup():
// Create a taskgraph nogroup, replay N times,
// and ensures the structured block is executed once.
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphNoGroup(void)
{
    int errors = 0;

    # define N 16
    int x = 0;
    int y = 0;
    #pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int i = 0 ; i < N ; ++i)
            {
                # pragma omp taskgraph nogroup
                {
                    ++x;
                    # pragma omp task shared(y)
                    {
                        # pragma omp atomic
                            ++y;
                    }
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 1);
    OMPVV_TEST_AND_SET_VERBOSE(errors, y != N);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphNoGroup());
    OMPVV_REPORT_AND_RETURN(errors);
}
