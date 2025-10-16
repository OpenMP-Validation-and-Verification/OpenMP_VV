//===-- test_task_transparent_import.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskTransparentImport():
// Ensure the correct order of execution an importing task and its uncle task
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

int testTaskTransparentImport(void)
{
    int errors = 0;
    int x = 0;

    #pragma omp parallel shared(x) num_threads(OMPVV_NUM_THREADS_HOST)
    {
        # pragma omp single
        {
            # pragma omp task shared(x, errors) depend(out: x)
            {
                OMPVV_TEST_AND_SET_VERBOSE(errors, x != 0);
                ++x;
            }

            # pragma omp task shared(x, errors) transparent(omp_import)
            {
                # pragma omp task shared(x, errors) depend(in: x)
                {
                    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 1);
                    ++x;
                }
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 2);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskTransparentImport());
    OMPVV_REPORT_AND_RETURN(errors);
}
