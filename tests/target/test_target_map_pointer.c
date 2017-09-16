// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===--- test_target_map_pointer.c - test target with map pointer p[:N] -----===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// This test check if it is possible to map an array and a pointer to that array,
// and then access the array through the pointer. It is necessary
// to specify the array size with [:N]. If offloading is used, the value of p[] 
// is copied over the device. The array will be updated inside de omp target
// region and compared afterwards
//
////===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>

#define N 1000

int main() {
  int compute_array[N], *p;
  int sum = 0, result = 0;
  int i, isHost = -1, exeW = -1;

  // Array initialization
  for (i = 0; i < N; i++)
    compute_array[i] = 0;
  p = &compute_array[0];

#pragma omp target data map(tofrom: compute_array, isHost)
#pragma omp target map(to: p[:N])
  {
    // Record where the computation was executed
    isHost = omp_is_initial_device();
    exeW = (isHost < 0) ? 1 : 0;// 1 = device, 0 = host 
  
    // Array modified through the pointer
    for (i = 0; i < N; i++)
      p[i] = i;
   
  } // end target

  // Result comparison
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
