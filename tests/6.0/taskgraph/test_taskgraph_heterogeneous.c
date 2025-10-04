//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgrapHeterogeneoush():
// Create a taskgraph, spawns 3 tasks
//  T1 -> T2 -> T3
// with (T1, T3) on device, (T2) on host.
//
// Also ensures that the structured block is executed once
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphHeterogeneous(void)
{
    int errors = 0;

    # define N 16
    int x = 0;
    int y = 0;

    # pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int i = 0 ; i < N ; ++i)
            {
                # pragma omp taskgraph
                {
                    ++x;

                    # pragma omp target nowait map(tofrom: y) depend(out: y)
                        ++y;

                    # pragma omp task depend(out: y)
                        ++y;

                    # pragma omp target nowait map(tofrom: y) depend(out: y)
                        ++y;
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x == 1);
    OMPVV_TEST_AND_SET_VERBOSE(errors, y == 3*N);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphHeterogeneous());
    OMPVV_REPORT_AND_RETURN(errors);
}
