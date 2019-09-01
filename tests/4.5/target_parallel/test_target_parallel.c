//===---- test_target_parallel.c - combined consutrct target and parallel  -------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test checks for the combined construct target and parallel. It allows to create a
// parallel region inside of the target device. 
//
//===----------------------------------------------------------------------------------===//
//
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ompvv.h"

#define MIN(a,b) (a > b ? b : a)

int test_target_parallel() {
  OMPVV_INFOMSG("test_target_parallel");

  int max_threads, scalar_num_threads;
  int *num_threads;
  int errors = 0;

  // We obtain the default number of threads in the target region
#pragma omp target map(from:max_threads)
  {
    max_threads = omp_get_max_threads();
  }

  OMPVV_INFOMSG("Max threads = %d", max_threads);

  // Warning if max  threads is just 1, parallel does nothing
  OMPVV_WARNING_IF(max_threads == 1, "The max number of threads creatable was 1. This is not a specifications error but we could not confirm the parallel region.");

  num_threads = (int *) malloc (sizeof(int) * max_threads); 

  for (int i = 0; i < max_threads; ++i) {
    num_threads[i] = -1;
  }

#pragma omp target parallel map(from:num_threads[0:max_threads], scalar_num_threads)
  {
#pragma omp master
    {
      scalar_num_threads = omp_get_num_threads();
    }
    int thread_id = omp_get_thread_num();
    num_threads[thread_id] = omp_get_num_threads();
  }

  //Warning if only 1 thread in region 2, parallel does nothing
  OMPVV_WARNING_IF(scalar_num_threads == 1, "The number of threads in the parallel region was 1. This is not a specifications error but we could not confirm the parallel region.");

  //Error if threads created exceeds max_threads
  OMPVV_TEST_AND_SET(errors, scalar_num_threads > max_threads);
  OMPVV_ERROR_IF(scalar_num_threads > max_threads, "The reported number of threads = %d is larger than the max num threads = %d reported previously", scalar_num_threads, max_threads);

  //Error if num_threads is inconsistent between threads
  for (int i = 0; i < (int) MIN(scalar_num_threads, max_threads); ++i) {
    OMPVV_TEST_AND_SET(errors, num_threads[i] != scalar_num_threads);
    OMPVV_ERROR_IF(num_threads[i] != scalar_num_threads, "The number of threads at threadID = %d was %d. Expected was %d.", i, num_threads[i], scalar_num_threads);
  }

  free(num_threads);

  return errors;
}

int main() {
  OMPVV_TEST_OFFLOADING;

  int errors = 0;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_parallel());

  OMPVV_REPORT_AND_RETURN(errors);
}
