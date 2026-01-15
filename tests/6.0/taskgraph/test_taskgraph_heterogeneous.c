//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphHeterogeneous():
// Create a taskgraph, spawns 3 dependent tasks
//  T1 -> T2 -> T3
// with (T1, T3) on device, (T2) on host.
//
// Ensures that
//   - the structured block is executed 0 to N times
//      - 0 if taskgraph record created prior to execution
//      - N if taskgraph record is never created
//   - 3*N tasks are executed/replayed
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

                    # pragma omp target nowait map(tofrom: y) depend(out: y) shared(y)
                        ++y;

                    # pragma omp task depend(out: y) shared(y)
                        ++y;

                    # pragma omp target nowait map(tofrom: y) depend(out: y) shared(y)
                        ++y;
                }
            }
        }
    }

    OMPVV_TEST_AND_SET_VERBOSE(errors, !(0 <= x && x <= N));
    OMPVV_TEST_AND_SET_VERBOSE(errors, y != 3*N);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphHeterogeneous());
    OMPVV_REPORT_AND_RETURN(errors);
}
