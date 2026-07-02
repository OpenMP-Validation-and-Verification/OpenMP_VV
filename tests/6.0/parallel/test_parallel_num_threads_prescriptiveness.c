//===------ test_parallel_num_threads_prescriptiveness.c ---------------------===//
//
// OpenMP API Version 6.0 November 2024
//
// ***********
// DIRECTIVE:parallel
// CLAUSE:num_threads, prescriptiveness modifier
// ***********
//
// This test checks the prescriptiveness modifier for the num_threads clause on the
// parallel directive. When prescriptiveness(strict) is specified, the implementation
// must respect the requested number of threads when dyn-var is false and sufficient
// threads are available. The test verifies that when OMP_DYNAMIC is false and
// num_threads(prescriptiveness(strict): N) is used, exactly N threads execute.
//
//===-------------------------------------------------------------------------===//

#include <omp.h>
#include "ompvv.h"

int test_num_threads_prescriptiveness() {
  int errors = 0;
  int actual_threads = 0;

  #pragma omp parallel num_threads(prescriptiveness(strict): OMPVV_NUM_THREADS_HOST)
  {
    #pragma omp single
    {
      actual_threads = omp_get_num_threads();
    }
  }

  OMPVV_TEST_AND_SET(errors, actual_threads != OMPVV_NUM_THREADS_HOST);
  OMPVV_INFOMSG_IF(actual_threads == OMPVV_NUM_THREADS_HOST,
                   "prescriptiveness(strict): Obtained %d threads as requested.", actual_threads);
  OMPVV_ERROR_IF(actual_threads != OMPVV_NUM_THREADS_HOST,
                 "prescriptiveness(strict): Expected %d threads, got %d.",
                 OMPVV_NUM_THREADS_HOST, actual_threads);
  return errors;
}

int main() {
  int errors = 0;
  omp_set_dynamic(0);
  OMPVV_TEST_AND_SET(errors, test_num_threads_prescriptiveness() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
