//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraph():
// Create a taskgraph, and ensures that the
// the structured block is executed only once,
// and that the task is replayed
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphReplay(void)
{
    int errors = 0;
    # define N 16
    # define M 16

    int x = 0;
    int y = 0;

    #pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int i = 0 ; i < N ; ++i)
            {
                # pragma omp taskgraph
                {
                    ++x;
                    for (int j = 0 ; j < M ; ++j)
                    {
                        # pragma omp task shared(y)
                        {
                            # pragma omp atomic
                            ++y;
                        }
                    }
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x == 1);
    OMPVV_TEST_AND_SET_VERBOSE(errors, y == M*N);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphReplay());
    OMPVV_REPORT_AND_RETURN(errors);
}
