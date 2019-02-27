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

#define N 1000

// Test for OpenMP 4.5 target data map(from: )
int test_map_from() {

  printf("test_map_from\n");

  int sum = 0, sum2 = 0, errors = 0, isHost = 0;

  // host arrays: heap and stack
  int *h_array_h = (int *)malloc(N * sizeof(int));
  int h_array_s[N];

#pragma omp target data map(from: h_array_h[0:N])  \
        map(from: h_array_s[0:N])
  {
#pragma omp target map(from: isHost)
    {
      isHost = omp_is_initial_device();
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
  errors = (N != sum) || (2*N != sum2);
  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum=%d, sum2=%d, N=%d\n", (isHost ? "host" : "device"), sum, sum2, N);

  return errors;
}

// Test for OpenMP 4.5 target data map(tofrom: ) 
int test_map_tofrom() {

  printf("test_map_tofrom\n");

  int sum = 0, sum2 = 0, isHost = 0, errors = 0;

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
#pragma omp target map(tofrom: isHost)
    { 
      isHost += omp_is_initial_device();
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
  errors = (N != sum) || (N != sum2);
  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum=%d, sum2=%d, N=%d\n", (isHost ? "host" : "device"), sum, sum2, N);

  return errors;
}

// Test for OpenMP 4.5 target data map(to: ) 
int test_map_to() {

  printf("test_map_to\n");

  int sum = 0, sum2 = 0, errors = 0, isHost = 0;
  
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
  int *d_ishost = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());

#pragma omp target data map(to: h_array_h[0:N])  \
        map(to: h_array_s[0:N]) 
  {
#pragma omp target is_device_ptr(d_array, d_array2, d_ishost) map(to: isHost)
    {
      d_ishost[0] = isHost + omp_is_initial_device();
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
  omp_target_memcpy(&isHost, d_ishost, sizeof(int), 0, 0,
                    omp_get_initial_device(), omp_get_default_device());
  // deallocating device arrays 
  omp_target_free(d_array, omp_get_default_device());
  omp_target_free(d_array2, omp_get_default_device());
  omp_target_free(d_ishost, omp_get_default_device());

  // checking errors
  for (int i = 0; i < N; ++i) {
    sum += h_array2_h[i];
    sum2 += h_array2_s[i];
  }

  free(h_array_h);
  free(h_array2_h);
  errors = (N != sum) || (N != sum2);
  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum=%d, sum2=%d, N=%d\n", (isHost ? "host" : "device"), sum, sum2, N);

  return errors;
}

// Test for OpenMP 4.5 target data map(to: ) and map(from:)
int test_map_to_from() {

  printf("test_map_to_from\n");

  int sum = 0, errors = 0, isHost = 0, isHost2 = 0;
  int *h_array_h = (int *)malloc(N * sizeof(int));
  int *h_array2_h = (int *)malloc(N * sizeof(int));

  for (int i = 0; i < N; ++i) {
    h_array_h[i] = 1;
    h_array2_h[i] = 0;
  }

#pragma omp target data map(to: h_array_h[0:N]) map(from: h_array2_h[0:N])  
  {
#pragma omp target map(from: isHost) map(to: isHost2)
    {
      isHost = isHost2 + omp_is_initial_device();
      for (int i = 0; i < N; ++i)
        h_array2_h[i] = h_array_h[i];
    } // end target 
  } // end target data

  // checking errors 
  for (int i = 0; i < N; ++i)
    sum += h_array2_h[i];

  free(h_array_h);
  free(h_array2_h);
  errors = N - sum;
  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum=%d, N=%d\n", (isHost ? "host" : "device"), sum, N);

  return errors;
}

// Test for OpenMP 4.5 target data map(alloc:)
int test_map_alloc() {

  puts("test_map_alloc");

  int sum = 0, errors = 0, isHost = 0;
  int *h_array_h = (int *)malloc(N*sizeof(int));

  // pointer arithmetic is not supported on the devices for
  // the device address returned by omp_target_alloc
  // section 3.5.1 omp_target_alloc. OpenMP API Version 4.5 Nov 2015
  int *d_sum = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());
  int *d_ishost = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());

#pragma omp target data map(alloc: h_array_h[0:N])
  {
#pragma omp target is_device_ptr(d_sum, d_ishost)
    {
      d_ishost[0] = omp_is_initial_device();
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
    omp_target_memcpy(&isHost, d_ishost, sizeof(int), 0, 0,
                                  omp_get_initial_device(),
                                  omp_get_default_device());
  } // end target data
  omp_target_free(d_sum, omp_get_default_device());
  omp_target_free(d_ishost, omp_get_default_device());

  free(h_array_h);
  errors = N - sum;
  if (!errors)
    printf("Test passed on %s\n", (isHost ? "host" : "device"));
  else
    printf("Test failed on %s: sum=%d, N=%d\n", (isHost ? "host" : "device"), sum, N);

  return errors;
}

int main() {

  int errors = 0;

  errors += test_map_from();
  errors += test_map_tofrom();
  errors += test_map_to();
  errors += test_map_to_from();
  errors += test_map_alloc();

  return errors;
}
