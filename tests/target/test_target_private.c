// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===--test_target_map_array_default.c - test default behavior of array map--===//
// 
// OpenMP API Version 4.5 Nov 2015
//
//Testing private clause with target directive
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>

#define N 1000

int main() {
  int compute_array[N];
  int errors = 0;
  int i,j, isHost = -1, tid;
  
 
  for (i=0; i<N; i++) 
    compute_array[i] = 0;

  omp_set_num_threads(4);
#pragma omp target map(tofrom:compute_array) map(tofrom: isHost) private(tid)
  {
    /*Record where the computation was executed*/
    isHost = omp_is_initial_device();
    #pragma omp parallel private(i,tid)
    {  
      tid = omp_get_thread_num();
      #pragma omp for
      for (i = 0; i < N; i++)
        compute_array[i] = tid;
    }
  } // End target

  for (i = 0; i < omp_get_num_threads(); i++)
    for (j = 0; j < N/omp_get_num_threads(); j++)
      if(compute_array[i*10+j] != i){
        errors += 1;
        printf("compute_array[%d] = %d\n",i*10+j,compute_array[i*10+j]);
      }

  if (errors) {
    printf("Test failed on %s\n",isHost ? "host":"device");
  }
  else {
    printf("Test passed on %s\n", isHost ? "host":"device");
  }
  return errors;

}
