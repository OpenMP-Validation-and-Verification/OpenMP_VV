//===-- test_taskgraph.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskTransparentImpex():
// Ensure the correct order of execution of cousin tasks, with impex parents
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <omp.h>
#include "ompvv.h"

int testTaskTransparentImpex(void)
{
    int errors = 0;
    int x = 0;

    #pragma omp parallel shared(x)
    {
        # pragma omp single
        {
            # pragma omp task shared(x, errors) transparent(omp_impex)
            {
                # pragma omp task shared(x, errors) depend(out: x)
                {
                    OMPVV_TEST_AND_SET_VERBOSE(errors, x == 0);
                    ++x;
                }
            }

            # pragma omp task shared(x, errors) transparent(omp_impex)
            {
                # pragma omp task shared(x, errors) depend(out: x)
                {
                    OMPVV_TEST_AND_SET_VERBOSE(errors, x == 1);
                    ++x;
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x == 2);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskTransparentImpex());
    OMPVV_REPORT_AND_RETURN(errors);
}
