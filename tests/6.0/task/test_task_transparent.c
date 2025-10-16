//===-- test_task_transparent.c ------------------------------------------------===//
//
// OpenMP API Version 6.0 Nov 2024
//
// Description
// testTaskTransparent():
// Test all arguments of the transparent clause
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

int testTaskTransparent(void)
{
    int errors = 0;
    int x = 0;

    #pragma omp parallel shared(x) num_threads(OMPVV_NUM_THREADS_HOST)
    {
        # pragma omp single
        {
            # pragma omp task shared(x) transparent
            {
                # pragma omp atomic
                    ++x;
            }

            # pragma omp task shared(x) transparent(omp_not_impex)
            {
                # pragma omp atomic
                    ++x;
            }

            # pragma omp task shared(x) transparent(omp_import)
            {
                # pragma omp atomic
                    ++x;
            }

            # pragma omp task shared(x) transparent(omp_export)
            {
                # pragma omp atomic
                    ++x;
            }

            # pragma omp task shared(x) transparent(omp_impex)
            {
                # pragma omp atomic
                    ++x;
            }
        }
    }
    OMPVV_TEST_AND_SET_VERBOSE(errors, x != 5);
    return errors;
}

int main(void)
{
    int errors = 0;
    OMPVV_TEST_AND_SET_VERBOSE(errors, testTaskTransparent());
    OMPVV_REPORT_AND_RETURN(errors);
}
