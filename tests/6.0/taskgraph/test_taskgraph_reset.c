//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphGraphReset():
// Create 'M' taskgraphs that spawns 3 tasks
//  T1 -> T2 -> T3
// with (T1, T3) on device, (T2) on host.
// Replay taskgraphs 'N' times, while reseting every second iterations
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphGraphReset(void)
{
    int errors = 0;

    # define N 16
    # define M 16
    _Static_assert(M % 2 == 0, "M must be even");

    int x[M];   // number of time the structued block executed for taskgraph i
    int y[M];   // number of time tasks executes for taskgraph i
    memset(x, 0, sizeof(x));
    memset(y, 0, sizeof(y));

    # pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int i = 0 ; i < N ; ++i)
            {
                for (int j = 0 ; j < M ; ++j)
                {
                    # pragma omp taskgraph graph_id(j) graph_reset(j % 2 == 0)
                    {
                        ++x[j];

                        # pragma omp target map(tofrom: y) depend(out: y)
                            ++x[j];

                        # pragma omp task depend(out: y)
                            ++x[j];

                        # pragma omp target map(tofrom: y) depend(out: y)
                            ++x[j];
                    }
                }
            }
        }
    }

    for (int j = 0 ; j < M ; ++j)
    {
        // each taskgraph strucuted block must have executed once
        OMPVV_TEST_AND_SET_VERBOSE(errors, x[j] == M/2);

        // each triplets must have executed N times
        OMPVV_TEST_AND_SET_VERBOSE(errors, y[j] == 3*N);
    }

    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphGraphReset());
    OMPVV_REPORT_AND_RETURN(errors);
}
