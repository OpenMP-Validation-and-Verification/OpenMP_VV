//===--- regression_threaded_kernel_launch.c ----------------------------------------===//
//
// OpenMP API Version 4.5
//
// Create N number of threads and simultaensouly launch
// various kernels. Ensure all kernels are executed correctly.
//
//
// Author: Aaron Jarmusch <jarmusch@udel.edu> Jun 2023
////===----------------------------------------------------------------------===//

#include <stdio.h>
#include "omp.h"
#include "ompvv.h"

#define N 1024 // NUmber of threads
#define NUM_THREADS 8 //
#define NUM_TEAMS 8 //
#define NUM_KERNELS 200 // Number of Kernels to launch

int kernel(int a, int b, int x)
{
    // Perform computations for the kernel
    int total = a + b;

    // printf("Kernel %d executed by thread %d\n", x, omp_get_thread_num());

    return total;
}

int test_omp_thread_kernel(void)
{
    int a[N];
    int b[N];
    int total = 0;
    int expect_total = 0;
    int errors = 0;
    int num_threads[N];
    int num_total[N];

    for (int x = 0; x < N; ++x) {
        a[x] = 1;
        b[x] = x;
        num_total[x] = 0;
        // num_threads[x] = -1;
    }

    // set the number of threads
    omp_set_num_threads(128);

    #pragma omp target data map(tofrom: num_total[0:NUM_KERNELS])
    {
        #pragma omp parallel for 

            for (int i = 0; i < NUM_KERNELS; ++i)
            {
                #pragma omp target parallel for reduction(+:num_total[i])

                    for (int x = 0; x < N; ++x) 
                    {
                        for (unsigned int y = 0; y < 0xfffff; ++y) {
                        // total += kernel(a[x], b[x], x);
                        num_total[i] += a[x] + b[x];
                        // while(1){
                        //     int c = c + 1;
                        // }
                        }
                    }

                // num_total[i] = total;
                // printf("hello");
            }

    }
    #pragma omp barrier 

    for (int i = 0; i < NUM_KERNELS; ++i){
        printf("%d\n", num_total[i]);
    }

    for (int x = 0; x < NUM_KERNELS; ++x) {
        expect_total += a[x] + b[x];
    }


    OMPVV_TEST_AND_SET_VERBOSE(errors, expect_total != total);
    printf("Total from loop directive is %d but expected total is %d.\n", total, expect_total);
    OMPVV_ERROR_IF(expect_total != total, "Total from loop directive is %d but expected total is %d.", total, expect_total);

    return errors;
}

int main(void)
{
    OMPVV_TEST_OFFLOADING;
    int errors = 0;

    OMPVV_TEST_AND_SET_VERBOSE(errors, test_omp_thread_kernel() != 0);

    // No error will be reported even if it is recorded.
    OMPVV_REPORT_AND_RETURN(errors);
}
