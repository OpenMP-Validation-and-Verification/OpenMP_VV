//===-- test_taskgraph_if.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphIf():
// Call
//  taskgraph if(1)
//  taskgraph if(0)
//  taskgraph if(1)
// and ensures that the structured block is executed only once,
// and records twice
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskgraphIf(void)
{
    int errors = 0;
    int x = 0;
    int y = 0;
    #pragma omp parallel shared(x, y)
    {
        # pragma omp single
        {
            for (int i = 0 ; i < 3 ; ++i)
            {
                # pragma omp taskgraph if(i != 1)
                {
                    ++x;

                    # pragma omp task
                        ++y;
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x == 1);
    OMPVV_TEST_AND_SET_VERBOSE(errors, y == 2);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphIf());
    OMPVV_REPORT_AND_RETURN(errors);
}
