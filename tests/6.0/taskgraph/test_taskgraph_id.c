//===-- test_taskgraph_id.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphId():
// M times, run N taskgraphs constructs with id '0<=i<N' that spawns 3 tasks
//  T1 -> T2 -> T3
// Ensure that each taskgraph' structured block executed 1 to M times
// Ensure that each triplets executed precisely M times
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphGraphId(void)
{
    int errors = 0;

    # define N 16
    # define M 16

    int x[N];
    int y[N];
    memset(x, 0, sizeof(x));
    memset(y, 0, sizeof(y));

    # pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int j = 0 ; j < M ; ++j)
            {
                for (int i = 0 ; i < N ; ++i)
                {
                    # pragma omp taskgraph graph_id(i)
                    {
                        ++x[i];
                        for (int k = 0 ; k < 3 ; ++k)
                        {
                            # pragma omp task depend(out: y) shared(y)
                            {
                                # pragma omp atomic
                                    ++y[i];
                            }
                        }
                    }
                }
            }
        }
    }

    for (int i = 0 ; i < N ; ++i)
    {
        // each taskgraph strucuted block must have executed 1 to M times
        OMPVV_TEST_AND_SET_VERBOSE(errors, !(0 < x[i] && x[i] <= M));

        // each triplets must have executed M times
        OMPVV_TEST_AND_SET_VERBOSE(errors, y[i] != 3*M);
    }

    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphGraphId());
    OMPVV_REPORT_AND_RETURN(errors);
}
