//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphParallel():
// N times, have 'nthreads' threads call the same taskgraph construct
//
// ensure the structured block executed 1 to N times,
// and the replayable task 'N*nthreads' times
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphParallel(void)
{
    int errors = 0;

    # define N 16
    int x = 0;
    int y = 0;
    int nthreads = -1;

    #pragma omp parallel shared(x, y, nthreads)
    {
        # pragma omp single
        {
            nthreads = omp_get_num_threads();
        }

        for (int i = 0 ; i < N ; ++i)
        {
            # pragma omp taskgraph
            {
#pragma omp atomic update
                ++x;
                # pragma omp task shared(y)
                {
                    # pragma omp atomic
                        ++y;
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, nthreads <= 0);
    OMPVV_TEST_AND_SET_VERBOSE(errors, !(0 < x && x <= N));
    OMPVV_TEST_AND_SET_VERBOSE(errors, y != nthreads*N);

    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphParallel());
    OMPVV_REPORT_AND_RETURN(errors);
}
