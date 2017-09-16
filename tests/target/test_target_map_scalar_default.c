// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

//===--- test_target_map_escalar_default.c - testing default map to scalar --===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// When no map-type-modifier (e.g. to, from and tofrom) are not specified, the 
// default behavior should be tofrom. This test check if this is sattisfied with
// a simple integer value. An array is created an initialized to zero in the host
// then changed in the device with a scalar value.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>

#define N 1000

int main() {
  int compute_array[N];
  int asclr = 12, sum = 0, result = 0;
  int i, isHost = -1, exeW = -1;

  // Array initialization
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;

#pragma omp target map(from: compute_array) map(tofrom: isHost) map(asclr)
  {
  // Record where the computation was executed
  isHost = omp_is_initial_device();
  exeW = (isHost < 0) ? 1 : 0; // 1 = device, 0 = host

  for (i = 0; i < N; i++)
    compute_array[i] = i + asclr;
 
  } // end target

  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];    
  
  for (i = 0; i < N; i++)
    result += i + asclr;

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
