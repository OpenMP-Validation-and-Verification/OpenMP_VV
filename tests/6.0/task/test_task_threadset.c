//--------------test_map-type_modifier_default.c-----------------------------------//
// OpenMP API Version 6.0 November 2024
// Pg. 901, line 30
// ***********
// DIRECTIVE:task
// CLAUSE:threadset
// ***********
// DESCRIPTION
//---------------------------------------------------------------------------------//

#include <omp.h>
#include "ompvv.h"

// Define a small number for testing purposes
#define N 1024

void task_work(int* arr) {
  #pragma omp parallel for
    for (int i = 0; i < N; ++i) {
      arr[i] = i;
    }
}

void check_work(int* arr, int* errors) {
    for (int i = 0; i < N; ++i) {
        if (arr[i] != i) {
            (*errors)++;
  }
}
}

int test_task_threadset() {
    int errors = 0;
    int arr1[N], arr2[N];

    // Test task with threadset(omp_pool)
    #pragma omp task threadset(omp_pool)
    {
        task_work(arr1);
        check_work(arr1, &errors);
    }

    #pragma omp taskwait
    OMPVV_TEST_VERBOSE(errors != 0);

    // Test task with default threadset (should be omp_team)
    #pragma omp task
    {
        task_work(arr2);
        check_work(arr2, &errors);
    }

    // Wait for all tasks to complete
    #pragma omp taskwait

    OMPVV_TEST_VERBOSE(errors != 0);

    return errors;
}

int main() {
    int errors = 0;

    // Test with threadset(omp_pool) and default threadset
    OMPVV_TEST_AND_SET(errors, test_task_threadset() != 0);

    OMPVV_REPORT_AND_RETURN(errors);
}