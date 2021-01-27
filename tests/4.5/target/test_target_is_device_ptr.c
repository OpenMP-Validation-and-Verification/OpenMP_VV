//===--- test_target_is_device_ptr.c--is_device_ptr clause on target directive--===//
//
// OpenMP API Version 4.5 Nov 2015
//
//  This test checks for the use of the is_device_ptr() clause on an array that 
//  is allocated with the omp_target_alloc() API call. If this test runs on the
//  host, we will warn that we won't be allocating on any device and the test 
//  will fail.
//
////===-------------------------------------------------------------------------===//


#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#include "ompvv.h"

#define N 10000

int main() {
  

  int isOffloading;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  
  OMPVV_WARNING_IF(!isOffloading, "This test is running on the host, the allocation of the memory returns a host pointer");

  int errors = 0;
  int *array_device = NULL;
  int *array_host = NULL;
  

  array_device = (int *) omp_target_alloc(N*sizeof(int), omp_get_default_device());
  if (array_device == NULL) {
    errors = 1; 
    OMPVV_ERROR("Array device is null: cannot allocate memory on device, is_device_ptr() cannot be properly tested");
    OMPVV_REPORT_AND_RETURN(errors);
  } 
 
  array_host = (int *) malloc(N*sizeof(int));

  for (int i = 0; i < N; ++i) {
    array_host[i] = i;
  }

#pragma omp target is_device_ptr(array_device) map(tofrom: array_host[0:N]) 
{
  for (int i = 0; i < N; ++i) {
    array_device[i] = i;
    array_host[i] += array_device[i];
  } 
} // end target

  // checking results
  for (int i = 0; i < N; ++i) {
    OMPVV_TEST_AND_SET_VERBOSE(errors, array_host[i] != 2*i);
  }

  omp_target_free(array_device, omp_get_default_device());
  free(array_host);

  OMPVV_REPORT_AND_RETURN(errors);
}
