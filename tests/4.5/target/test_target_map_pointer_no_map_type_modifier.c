//===--- test_target_map_pointer_no_map_type_modifier.c -----------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// When map-type-modifier (e.g. to, from and tofrom) are not specified, the 
// default behavior should be tofrom. This test checks if this is the case by
// creating an array and a pointer to the array. The array and pointer are mapped
// to the device. The mapped pointer should point to the device array when used
// inside the omp target region. The array is then changed through the pointer.
// Array is mapped as tofrom, while pointer is mapped with default value.  
// 
////===------------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 1000

// Test that maptype of non-scalar pointer in map clause defaults to tofrom 
int test_default_tofrom() {
  int compute_array[N];
  int *p;	
  int sum = 0, result = 0, errors = 0;
  int i;
 
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;
  
  p = &compute_array[0];

#pragma omp target map(p[0:N])
  {
    for (i = 0; i < N; i++)
      p[i] = i;
  } // end target

  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];    

  for (i = 0; i < N; i++)
    result += i;

  OMPVV_TEST_AND_SET_VERBOSE(errors, result != sum);
  
  return errors; 
}

int main() {
  int errors = 0;
    
  int isOffloading;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_default_tofrom());

  OMPVV_REPORT_AND_RETURN(errors);
}
