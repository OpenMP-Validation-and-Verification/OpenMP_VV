//===--test_target_private.c ------------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
//Testing private clause with target directive
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10

int main() {

  int compute_array[4][N];
  int errors = 0;
  int i, j;

  //Check for offloading
  int is_offloading;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);

  for (i = 0; i < 4; i++) 
    for (j = 0; j < N; j++) 
      compute_array[i][j] = 0;


  omp_set_num_threads(4);
#pragma omp parallel
{
  int fp_val = omp_get_thread_num();
  int p_val=0;

#pragma omp target map(tofrom:compute_array) firstprivate(fp_val) private(p_val)
  {
    p_val = fp_val;

    for (i = 0; i < N; i++)
      compute_array[p_val][i] = 100;
  } // end target
}//end parallel

  for (i = 0; i < 4; i++){ 
    for (j = 0; j < N; j++){
      OMPVV_TEST_AND_SET_VERBOSE(errors, (compute_array[i][j] != 100));
      if (compute_array[i][j] != 100) 
        printf("compute_array[%d][%d] = %d\n",i,j,compute_array[i][j]);
     
    }
  }//end for
  
  OMPVV_REPORT_AND_RETURN(errors);

}

