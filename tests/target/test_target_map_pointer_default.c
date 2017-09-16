// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===--- test_target_map_pointer.c - test default behavior pointer mapping --===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// When no map-type-modifier (e.g. to, from and tofrom) are not specified, the 
// default behavior should be tofrom. This test check if this is the case by
// creating an array and a pointer to the array. The array and pointer are mapped
// to the device. The mapped pointer should point to the device array when used
// inside the omp target region. The array is then changed through the pointer.
// Array is mapped as tofrom, while pointer is mapped with default value
//
////===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>

#define N 1000

int main() {
  int compute_array[N], *p;
  int sum = 0, result = 0;
  int i, isHost = -1, exeW = -1;
 
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;
  p = &compute_array[0];

// p is mapped as a pointer to compute_array
#pragma omp target data map(tofrom: compute_array, isHost)
#pragma omp target map(p[0:0])
  {
    // Record where the computation was executed
    isHost = omp_is_initial_device();
    exeW = (isHost < 0) ? 1 : 0; // 1 = device, 0 = host

    for (i = 0; i < N; i++)
      p[i] = i;
  } // end target

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
