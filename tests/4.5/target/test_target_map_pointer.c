//===--- test_target_map_pointer.c ------------------------------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
//
// This test check if it is possible to map an array and a pointer to that array,
// and then access the array through the pointer. It is necessary
// to specify the array size with [:N]. If offloading is used, the value of p[] 
// is copied over the device. The array will be updated inside the omp target
// region and compared afterwards
//
////===----------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 1000

int main() {
  int compute_array[N];
  int *p;
  int sum = 0, result = 0, errors = 0;
  int i;
  
  
  // Array initialization
  for (i = 0; i < N; i++)
    compute_array[i] = 0;
  p = &compute_array[0];

  int isOffloading;

  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);

  OMPVV_WARNING_IF(!isOffloading, "This test is running on host, the value of p[] is not copied over to the device"); 

#pragma omp target data map(tofrom: compute_array) //To test default pointer behavior, array must be mapped before the pointer
#pragma omp target map(to: p[:N]) 
  {
    // Array modified through the pointer
    for (i = 0; i < N; i++)
      p[i] = i;
   
  } // end target

  // Result comparison
  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];    
  
  for (i = 0; i < N; i++)
    result += i;

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != sum);

  OMPVV_REPORT_AND_RETURN(errors);

}
