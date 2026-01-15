//===-- test_taskgraph_if.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskgraphIf():
// Call
//  taskgraph if(0) --> execute taskgraph construct, optionally creating a taskgraph record
//  taskgraph if(1) --> skip
//  taskgraph if(2) --> execute taskgraph construct, maybe replay taskgraph
//                      record, or optionally create a taskgraph record
// ensures that the structured block is executed 1 or 2 times
// ensures that tasks are executed twice
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

                    # pragma omp task shared(y)
                        ++y;
                }
            }
        }
    }

    // the structured block must have been executed once or twice
    // (once if creating a taskgraph record, twice if creating no taskgraph record)
    OMPVV_TEST_AND_SET_VERBOSE(errors, !(x == 1 || x == 2));

    // the task must execute precisely twice
    OMPVV_TEST_AND_SET_VERBOSE(errors, y != 3);

    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskgraphIf());
    OMPVV_REPORT_AND_RETURN(errors);
}
