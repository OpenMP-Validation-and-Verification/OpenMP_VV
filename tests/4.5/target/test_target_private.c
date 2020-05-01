//===--test_target_private.c ----------------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// Testing private clause with target directive. The test begins by initializing
// and filling a 2-D array with all zeros and generating four threads. In a parallel
// region inside the target region, the mapped 2-D array is assigned the integer 
// value equal to the thread number assigned to the private variable.
// Finally, back on the host, we check that array is properly filled.
//
////===--------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10

int main() {

  int compute_array[4][N];
  int errors = 0;
  int i, j;
  int real_num_threads;
  int p_val= -1;

  //Check for offloading
  int is_offloading;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);

  for (i = 0; i < 4; i++) 
    for (j = 0; j < N; j++) 
      compute_array[i][j] = 0;


#pragma omp target map(tofrom: compute_array) (from: real_num_threads) private(p_val)
{
#pragma omp parallel private(p_val,i) num_threads(4)
  {
    p_val = omp_get_thread_num();
    real_num_threads = omp_get_num_threads();
    OMPVV_WARNING_IF(real_num_threads == 1, "The number of threads creatable was 1. This is not a specifications error but we could not confirm privatization.");
    for (i = 0; i < N; i++)
      compute_array[p_val][i] += p_val;
  } // end parallel
}//end target

  for (i = 0; i < real_num_threads; i++){ 
    for (j = 0; j < N; j++){
      OMPVV_TEST_AND_SET_VERBOSE(errors, (compute_array[i][j] != i));
    }
  }//end for
  
  OMPVV_REPORT_AND_RETURN(errors);

}

