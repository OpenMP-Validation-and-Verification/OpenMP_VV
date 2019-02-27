//===--test_target_firstprivate.c ------------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
//Testing first private clause with target directive
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10
#define NUM_THREADS 10

int main() {
  int compute_array[NUM_THREADS][N];
  int errors = 0;
  int i,j;
  int actualNumThreads;

  OMPVV_TEST_OFFLOADING;
 
  for (i=0; i<NUM_THREADS; i++) 
    for (j=0; j<N; j++) 
      compute_array[i][j] = 0;

  omp_set_num_threads(NUM_THREADS);
#pragma omp parallel
{
  int p_val = omp_get_thread_num();
  actualNumThreads = omp_get_num_threads();

#pragma omp target map(tofrom:compute_array) firstprivate(p_val)
  {
    for (i = 0; i < N; i++)
      compute_array[p_val][i] = 100;
    // Checking if the value is not copied back
    p_val++;
  } // End target
  if (p_val == omp_get_thread_num()) {
    for (i = 0; i < N; i++)
      compute_array[p_val][i]++;
  }
}//end-parallel

  OMPVV_WARNING_IF(actualNumThreads == 1, "The number of threads in the host is 1. This tests is inconclusive")
  for (i=0; i<actualNumThreads; i++){ 
    for (j=0; j<N; j++){
      OMPVV_TEST_AND_SET(errors, compute_array[i][j] != 101);
      OMPVV_ERROR_IF(compute_array[i][j] == 100, "p_val changed after target region for thread %d",i)
    }
  }//end-for

  OMPVV_REPORT_AND_RETURN(errors);
}
