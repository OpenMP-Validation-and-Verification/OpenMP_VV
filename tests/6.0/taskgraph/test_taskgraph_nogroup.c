//===-- test_taskgraph_nogroup.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphNoGroup():
// Run a taskgraph nogroup construct N times with 1 task.
// Ensures the structured block is executed from 0 to N times
// Ensures the task is executed N times
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
    OMPVV_TEST_AND_SET_VERBOSE(errors, !(0 <= x && x <= N));
    OMPVV_TEST_AND_SET_VERBOSE(errors, y != N);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphNoGroup());
    OMPVV_REPORT_AND_RETURN(errors);
}
