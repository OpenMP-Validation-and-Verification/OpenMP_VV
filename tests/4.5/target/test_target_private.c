//===--test_target_private.c ----------------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// Testing private clause with target directive. The test begins by initializing
// and filling a 2-D array with all zeros and generating four threads. In a parallel
// region inside the target region, the mapped 2-D array is assigned the integer 
// value equal to the thread number  assigned to the private variable.
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


  omp_set_num_threads(4);
  real_num_threads = omp_get_num_threads();  

#pragma omp target map(tofrom:compute_array) private(p_val)
{
#pragma omp parallel
  {
    p_val = omp_get_thread_num();
#pragma omp for
    for (i = 0; i < N; i++)
      compute_array[p_val][i] += p_val;
  } // end parallel
}//end target

  for (i = 0; i < real_num_threads; i++){ 
    for (j = 0; j < N; j++){
      OMPVV_TEST_AND_SET_VERBOSE(errors, (compute_array[i][j] != i));
      OMPVV_ERROR_IF(compute_array[i][j] != 100, "compute_array[%d][%d] = %d\n",i,j,compute_array[i][j]);
    }
  }//end for
  
  OMPVV_REPORT_AND_RETURN(errors);

}

