//===--- test_target_map_local_array.c --------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// This test will check if an array that is declared and initialized in the host, 
// can be copied over the device, updated there, and then copied back. If this test 
// runs on the host, we will warn that array is not allocated on any device.
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h"

#define N 10000

int main() {
  //define compute array locally
  int compute_array[N];
  int sum = 0, errors = 0, result = 0;
  int i;

  // Host initialization of the array 
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;

  int isOffloading;

  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);

  OMPVV_WARNING_IF(!isOffloading, "This test is running on host, array is not allocated on device");

#pragma omp target map(tofrom: compute_array[0:N])
  {
  	
  // Updating the compute_array
  for (i = 0; i < N; i++)
      compute_array[i] = i;
   
  } // end target

  // Comparing the results
  for (i = 0; i < N; i++){
    sum = sum + compute_array[i];    
    result += i;
  }
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, result != sum);

  OMPVV_REPORT_AND_RETURN(errors);

}
