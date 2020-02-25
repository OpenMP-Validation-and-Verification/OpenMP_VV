//===--- test_target_map_scalar_no_map_type_modifier.c ----------------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// When map-type-modifier (e.g. to, from and tofrom) is not specified, the 
// default behavior should be tofrom. This test checks if this is satisfied with
// a simple integer value. An array is created an initialized to zero in the host
// then changed in the device with a scalar value. An additional simple test 
// function also checks from behavior by changing scalar value on device.
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include "ompvv.h" 

#define N 1000

int test_scalar_to() {

  int compute_array[N];
  int asclr = 12, sum = 0, result = 0, errors = 0;
  int i;

  // Array initialization
  for (i = 0; i < N; i++) 
    compute_array[i] = 0;

#pragma omp target map(from: compute_array) map(asclr)
  {
  for (i = 0; i < N; i++)
    compute_array[i] = i + asclr;
 
  } // end target

  for (i = 0; i < N; i++)
    sum = sum + compute_array[i];    
  
  for (i = 0; i < N; i++)
    result += i + asclr;

  OMPVV_TEST_AND_SET_VERBOSE(errors, result!= sum);  

  return errors;
}

int test_scalar_from() {

  int new_scalar = 25;
  int errors = 0;

#pragma omp target map(new_scalar)
  {
  //Change scalar value on device
  new_scalar = 27;

  }
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, new_scalar!=27);
  
  return errors;	
}

int main() {
  
  int errors = 0;

  //Test Offloading
  int is_offloading;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);
  OMPVV_WARNING_IF(!is_offloading, "This test in running on host, asclr is not copied over to the device");

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_scalar_to());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_scalar_from());  
  OMPVV_REPORT_AND_RETURN(errors);
}
