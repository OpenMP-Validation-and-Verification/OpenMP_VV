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
int compute_array[N];

int main() {
  int sum = 0, result = 0;
  int i, isHost = -1;

  // Host initialization of the array 
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;

#pragma omp target map(from: compute_array) map(tofrom: isHost)
  {
    // Record where the computation was executed
    isHost = omp_is_initial_device();
	
	  // Updating the compute_array
    for (i = 0; i < N; i++)
      compute_array[i] = i;
   
  } // end target

  // Comparing the results
  for (i = 0; i < N; i++){
    sum = sum + compute_array[i];    
    result += i;
  }

  if (result != sum) {
    printf("Test failed on %s\n",isHost ? "host":"device");
    return 1;
  }
  else {
    printf("Test passed on %s\n",isHost? "host":"device");
    return 0;
  }
}
