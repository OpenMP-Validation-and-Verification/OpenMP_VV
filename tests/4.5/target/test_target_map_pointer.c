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
  int i, isHost = -1;

  // Array initialization
  for (i = 0; i < N; i++)
    compute_array[i] = 0;
  p = &compute_array[0];

#pragma omp target data map(tofrom: compute_array) //To test default pointer behavior, array must be mapped before the pointer
#pragma omp target map(to: p[:N]) map(tofrom: isHost)
  {
    // Record where the computation was executed
    isHost = omp_is_initial_device();
  
    // Array modified through the pointer
    for (i = 0; i < N; i++)
      p[i] = i;
   
  } // end target

  // Result comparison
  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];    
  
  for (i = 0; i < N; i++)
    result += i;

  if (result != sum) {
    printf("Test failed on %s\n", isHost ? "host":"device");
    return 1;
  }
  else {
    printf("Test passed on %s\n", isHost ? "host":"device");
    return 0;
  }

}
