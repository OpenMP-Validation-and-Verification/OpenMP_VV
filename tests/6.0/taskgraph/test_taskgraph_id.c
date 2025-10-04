//===-- test_taskgraph_id.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphId():
// Create 'N' taskgraphs with id '0<=i<N' that spawns 3 tasks
//  T1 -> T2 -> T3
//
// Replay each taskgraph 'M' times
//
// Ensure that each strucuted block only executed once
// Ensure that each triplets evexecuted 'N' times
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

    int x = 0;
    int y = 0;

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
    }

    // each taskgraph strucuted block must have executed once
    OMPVV_TEST_AND_SET_VERBOSE(errors, x == N);

    // each triplets must have executed N times
    OMPVV_TEST_AND_SET_VERBOSE(errors, y == 3*N*M);

    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphGraphId());
    OMPVV_REPORT_AND_RETURN(errors);
}
