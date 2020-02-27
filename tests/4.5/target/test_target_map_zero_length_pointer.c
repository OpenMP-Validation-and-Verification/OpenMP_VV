//===--- test_target_map_zero_length_pointer.c -----------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// If a pointer is referenced in a target construct, not declared in the target 
// construct, and does not appear in a data-sharing attribute or map clause, it
// is treated as if it appeared in a map clause as a zero-length array section. 
// This test checks this rule by giving a pointer the address of an array, 
// mapping that array to a device with tofrom map-type, and then changing the 
// values of the array on the device using the pointer. Finally, back on the 
// host the array is checked in order to confirm that values were properly 
// modified through the pointer.
//
////===---------------------------------------------------------------------===//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define N 1000

//Test non-specified mapping of pointer as a zero-length array in a map clause
int test_zero_length_pointer() {
  int compute_array[N];
  int *p;
  int sum = 0, result = 0, errors = 0;
  int i;
 
  for (i = 0; i < N; i++)
    compute_array[i] = 0;
  
  p = &compute_array[0];

#pragma omp target data map(tofrom: compute_array)
#pragma omp target
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
 
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_zero_length_pointer());

  OMPVV_REPORT_AND_RETURN(errors);
}
