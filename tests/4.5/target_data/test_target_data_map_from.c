//===---- test_target_data_map.c - test for map type modifiers ------------===//
// 
// OpenMP API Version 4.5 Nov 2015
// 
// This test check all the possible map-type-modifiers for the target data map
// clauses. These are: from, to, fromto, alloc, release and delete. There 
// is a function for each test. 
//
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1000

// Test for OpenMP 4.5 target data map(from: )
int test_map_from() {

  OMPVV_INFOMSG("test_map_from");

  int sum = 0, sum2 = 0, errors = 0;

  // host arrays: heap and stack
  int *h_array_h = (int *)malloc(N * sizeof(int));
  int h_array_s[N];

#pragma omp target data map(from: h_array_h[0:N])  \
        map(from: h_array_s[0:N])
  {
#pragma omp target
    {
      for (int i = 0; i < N; ++i) {
        h_array_h[i] = 1;
        h_array_s[i] = 2;
      }
    } // end target
  } // end target data

  // checking results
  for (int i = 0; i < N; ++i) {
    sum += h_array_h[i];
    sum2 += h_array_s[i];
  }
  
  free(h_array_h);
  OMPVV_TEST_AND_SET_VERBOSE(errors, (N != sum) || (2*N != sum2));

  return errors;
}

// Test for OpenMP 4.5 target data map(tofrom: ) 
int test_map_tofrom() {

  OMPVV_INFOMSG("test_map_tofrom");

  int sum = 0, sum2 = 0, errors = 0;

  // host arrays: heap and stack
  int *h_array_h = (int *)malloc(N * sizeof(int));
  int h_array_s[N];

  for (int i = 0; i < N; ++i) {
    h_array_h[i] = 0;
    h_array_s[i] = 0;
  }

#pragma omp target data map(tofrom: h_array_h[0:N])    \
        map(tofrom : h_array_s[0:N]) 
  {
#pragma omp target //map //remove map clause?
    { 
      for (int i = 0; i < N; ++i) {
        h_array_h[i] += 1;
        h_array_s[i] += 1;
      }
    } // end target
  } // end target data 

   // checking errors 
  for (int i = 0; i < N; ++i) {
    sum += h_array_h[i];
    sum2 += h_array_s[i];
  }

  free(h_array_h);
  OMPVV_TEST_AND_SET_VERBOSE(errors, (N != sum) || (N != sum2));

  return errors;
}

// Test for OpenMP 4.5 target data map(to: ) 
int test_map_to() {

  OMPVV_INFOMSG("test_map_to");

  int sum = 0, sum2 = 0, errors = 0;
  
  // host arrays: heap and stack
  int *h_array_h = (int *)malloc(N*sizeof(int));
  int *h_array2_h = (int *)malloc(N*sizeof(int));
  int h_array_s[N];
  int h_array2_s[N];

  // initializing arrays 
  for (int i = 0; i < N; ++i) {
    h_array_h[i] = 1;
    h_array_s[i] = 1;
    h_array2_h[i] = 0;
    h_array2_s[i] = 0;
  }

  // device arrays to get the data from the device
  // pointer arithmetic is not supported on the devices for
  // the device address returned by omp_target_alloc
  // section 3.5.1 omp_target_alloc. OpenMP API Version 4.5 Nov 2015
  int *d_array =
      (int *)omp_target_alloc(N*sizeof(int), omp_get_default_device());
  int *d_array2 =
      (int *)omp_target_alloc(N*sizeof(int), omp_get_default_device());

#pragma omp target data map(to: h_array_h[0:N])  \
        map(to: h_array_s[0:N]) 
  {
#pragma omp target is_device_ptr(d_array, d_array2)
    {
      for (int i = 0; i < N; ++i) {
        d_array[i] = h_array_h[i];
        d_array2[i] = h_array_s[i];
      }
    } // end target
  } // end target data

  // copy from d to h
  omp_target_memcpy(h_array2_h, d_array, N*sizeof(int), 0, 0,
                    omp_get_initial_device(), omp_get_default_device());
  omp_target_memcpy(h_array2_s, d_array2, N*sizeof(int), 0, 0,
                    omp_get_initial_device(), omp_get_default_device());
  // deallocating device arrays 
  omp_target_free(d_array, omp_get_default_device());
  omp_target_free(d_array2, omp_get_default_device());

  // checking errors
  for (int i = 0; i < N; ++i) {
    sum += h_array2_h[i];
    sum2 += h_array2_s[i];
  }

  free(h_array_h);
  free(h_array2_h);
  OMPVV_TEST_AND_SET_VERBOSE(errors, (N != sum) || (N != sum2));

  return errors;
}

// Test for OpenMP 4.5 target data map(to: ) and map(from:)
int test_map_to_from() {

  OMPVV_INFOMSG("test_map_to_from");

  int sum = 0, errors = 0;
  int *h_array_h = (int *)malloc(N * sizeof(int));
  int *h_array2_h = (int *)malloc(N * sizeof(int));

  for (int i = 0; i < N; ++i) {
    h_array_h[i] = 1;
    h_array2_h[i] = 0;
  }

#pragma omp target data map(to: h_array_h[0:N]) map(from: h_array2_h[0:N])  
  {
#pragma omp target 
    {
      for (int i = 0; i < N; ++i)
        h_array2_h[i] = h_array_h[i];
    } // end target 
  } // end target data

  // checking errors 
  for (int i = 0; i < N; ++i)
    sum += h_array2_h[i];

  free(h_array_h);
  free(h_array2_h);

  OMPVV_TEST_AND_SET_VERBOSE(errors, ((N - sum) != 0));

  return errors;
}

// Test for OpenMP 4.5 target data map(alloc:)
int test_map_alloc() {

  OMPVV_INFOMSG("test_map_alloc");

  int sum = 0, errors = 0;
  int *h_array_h = (int *)malloc(N*sizeof(int));

  // pointer arithmetic is not supported on the devices for
  // the device address returned by omp_target_alloc
  // section 3.5.1 omp_target_alloc. OpenMP API Version 4.5 Nov 2015
  int *d_sum = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());
  
#pragma omp target data map(alloc: h_array_h[0:N])
  {
#pragma omp target is_device_ptr(d_sum)
    {
      for (int i = 0; i < N; ++i) 
        h_array_h[i] = 1;
      
      // checking errors
      d_sum[0] = 0; 
      for (int i = 0; i < N; ++i)
        d_sum[0] += h_array_h[i];
    } // end target
    omp_target_memcpy(&sum, d_sum, sizeof(int), 0, 0,
                                  omp_get_initial_device(),
                                  omp_get_default_device());
  } // end target data
  omp_target_free(d_sum, omp_get_default_device());

  free(h_array_h);
  OMPVV_TEST_AND_SET_VERBOSE(errors, (N - sum) != 0);

  return errors;
}

int main() {

  int errors = 0;
  
  int is_offloading;
  OMPVV_TEST_AND_SET_OFFLOADING(is_offloading);

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_map_from());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_map_tofrom());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_map_to());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_map_to_from());
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_map_alloc());

  OMPVV_REPORT_AND_RETURN(errors);
}
