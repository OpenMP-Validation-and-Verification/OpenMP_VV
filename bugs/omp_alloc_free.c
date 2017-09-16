// RUN: %libomptarget-compile-run-and-check-aarch64-unknown-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-powerpc64le-ibm-linux-gnu
// RUN: %libomptarget-compile-run-and-check-x86_64-pc-linux-gnu

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>

#define N 1000

/* Test for OpenMP 4.5 target data map(to: ) */
int test_map_to() {
  puts("test_map_to");
  int sum = 0, sum2 = 0, errors = 0, isHost = 0;
  
  /* host arrays: heap and stack */
  int *h_array_h = (int *)malloc(N * sizeof(int));
  int *h_array2_h = (int *)malloc(N * sizeof(int));
  int h_array_s[N];
  int h_array2_s[N];

  /* initializing arrays */
  for (int i = 0; i < N; ++i) {
    h_array_h[i] = 1;
    h_array_s[i] = 1;
    h_array2_h[i] = 0;
    h_array2_s[i] = 0;
  }

  /* device arrays to get the data from the device */
  /* pointer arithmetic is not supported on the devices for*/
  /* the device address returned by omp_target_alloc*/
  /* section 3.5.1 omp_target_alloc. OpenMP API Version 4.5 Nov 2015*/
  int *d_array =
      (int *)omp_target_alloc(N * sizeof(int), omp_get_default_device());
  int *d_array2 =
      (int *)omp_target_alloc(N * sizeof(int), omp_get_default_device());
  int *d_ishost = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());

#pragma omp target data map(to : h_array_h[0 : N])  \
    map(to : h_array_s[0 : N]) 
  {
#pragma omp target is_device_ptr(d_array, d_array2, d_ishost) map(to: isHost)
    {
      /* OpenMP API v4.5 November 2015. Line 14. The omp_target_alloc routine returns NULL if it cannot dynamically allocate the memory in the device data environment. */
      if (omp_is_initial_device()) {
        if (d_array != NULL)
          printf("d_array should be NULL, addr: %p\n", d_array);
        if (d_array2 != NULL)
          printf("d_array2 should be NULL, addr: %p\n", d_array2);
        if (d_ishost != NULL)
          printf("d_ishost should be NULL, addr: %p\n", d_ishost);
      }
      d_ishost[0] = isHost + omp_is_initial_device();
      for (int i = 0; i < N; ++i) {
        d_array[i] = h_array_h[i];
        d_array2[i] = h_array_s[i];
      }
    } /* end target */
  } /* end target data*/

  /* copy from d to h */
  omp_target_memcpy(h_array2_h, d_array, N * sizeof(int), 0, 0,
                    omp_get_initial_device(), omp_get_default_device());
  omp_target_memcpy(h_array2_s, d_array2, N * sizeof(int), 0, 0,
                    omp_get_initial_device(), omp_get_default_device());
  omp_target_memcpy(&isHost, d_ishost, sizeof(int), 0, 0,
                    omp_get_initial_device(), omp_get_default_device());
  /* deallocating device arrays */
  omp_target_free(d_array, omp_get_default_device());
  omp_target_free(d_array2, omp_get_default_device());
  omp_target_free(d_ishost, omp_get_default_device());

  /* checking errors */
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

/* Test for OpenMP 4.5 target data map(alloc:) */
int test_map_alloc() {
  puts("test_map_alloc");

  int sum = 0, errors = 0, isHost = 0;
  int *h_array_h = (int *)malloc(N * sizeof(int));

  /* pointer arithmetic is not supported on the devices for*/
  /* the device address returned by omp_target_alloc*/
  /* section 3.5.1 omp_target_alloc. OpenMP API Version 4.5 Nov 2015*/
  int *d_sum = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());
  int *d_ishost = (int *)omp_target_alloc(sizeof(int), omp_get_default_device());

#pragma omp target data map(alloc : h_array_h[0 : N])
  {
#pragma omp target is_device_ptr(d_sum, d_ishost)
    {
      /* OpenMP API v4.5 November 2015. Line 14. The omp_target_alloc routine returns NULL if it cannot dynamically allocate the memory in the device data environment. */
      if (omp_is_initial_device()) {
        if (d_sum != NULL)
          printf("d_sum should be NULL, addr: %p\n", d_sum);
        if (d_ishost != NULL)
          printf("d_ishost should be NULL, addr: %p\n", d_ishost);
      }
      d_ishost[0] = omp_is_initial_device();
      for (int i = 0; i < N; ++i) 
        h_array_h[i] = 1;
      
      /* checking errors*/
      d_sum[0] = 0; 
      for (int i = 0; i < N; ++i)
        d_sum[0] += h_array_h[i];
    } /* end target*/
    omp_target_memcpy(&sum, d_sum, sizeof(int), 0, 0,
                                  omp_get_initial_device(),
                                  omp_get_default_device());
    omp_target_memcpy(&isHost, d_ishost, sizeof(int), 0, 0,
                                  omp_get_initial_device(),
                                  omp_get_default_device());
  } /* end target data */
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

  errors += test_map_to();
  errors += test_map_alloc();

  return errors;
}
