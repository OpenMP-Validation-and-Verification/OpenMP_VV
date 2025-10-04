//===-- test_taskgraph_reset.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphGraphReset():
// Create 'M' taskgraphs that spawns 3 tasks
//  T1 -> T2 -> T3
// with (T1, T3) on device, (T2) on host.
// Replay taskgraphs 'N' times, while reseting every second iterations.
// 
// Ensures that structured block executed 'N/2' times
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
    _Static_assert(N % 2 == 0, "N must be even");

    int x = 0;
    int y = 0;

    # pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int i = 0 ; i < N ; ++i)
            {
                # pragma omp taskgraph graph_reset(i % 2 == 0)
                {
                    # pragma omp atomic
                        ++x;

                    for (int i = 0 ; i < 3 ; ++i)
                    {
                        # pragma omp task depend(out: y) shared(y)
                        {
                            # pragma omp atomic
                                ++y;
                        }
                    }
                }
            }
        }
    }

    // each taskgraph strucuted block must have executed once
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != N/2);

    // each triplets must have executed N times
    OMPVV_TEST_AND_SET_VERBOSE(errors, y != 3*N);

    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphGraphReset());
    OMPVV_REPORT_AND_RETURN(errors);
}
