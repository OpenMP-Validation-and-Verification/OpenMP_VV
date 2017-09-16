// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===--- test_target_map_array.c - test mapping an array to the device ------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// This test will check if an array that is declared and initialized in the host, 
// can be copied over the device, updated there, and then copied back. 
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>

#define N 1000

int main() {
  int compute_array[N];
  int sum = 0, result = 0;
  int i, isHost = -1, exeW = -1;

  // Host initialization of the array 
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;

#pragma omp target map(from: compute_array) map(tofrom: isHost)
  {
    // Record where the computation was executed
    isHost = omp_is_initial_device();
    exeW = (isHost < 0)? 1 : 0; // 1 = device, 0 = host 
	
	  // Updating the compute_array
    for (i = 0; i < N; i++)
      compute_array[i] = i;
   
  } // end target

  // Comparing the results
  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];    
  
  for (i = 0; i < N; i++)
    result += i;

  if (result == sum) {
    if (exeW)
      printf("Test passed on device\n"); 
    else 
      printf("Test passed on host\n"); 
    return 0;
  }
  else {
    if (exeW)
      printf("Test failed on device\n"); 
    else 
      printf("Test failed on host\n"); 

    return 1;
  }

  return 0;
}
