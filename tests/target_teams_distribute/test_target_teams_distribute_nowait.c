#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

// Test for OpenMP 4.5 target data with if
int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[1024];
  int b[1024];
  int c[1024];
  int d[1024];
  int e[1024];
  int f[1024];
  int g[1024];
  int errors = 0;
  int is_host;

  // a and b array initialization
  for (int x = 0; x < 1024; ++x) {
      a[x] = x;
      b[x] = 2 * x;
      c[x] = 0;
      d[x] = 3 * x;
      e[x] = 4 * x;
      f[x] = 0;
      g[x] = 0;
  }

  #pragma omp target data map(to: a[0:1024], b[0:1024], d[0:1024], e[0:1024], is_host) map(from: c[0:1024], f[0:1024], g[0:1024])
  {
      #pragma omp parallel
      {
          #pragma omp target teams distribute nowait
          for (int x = 0; x < 1024; ++x){
              c[x] = a[x] + b[x];
          }
          #pragma omp target teams distribute nowait
          for (int x = 0; x < 1024; ++x){
              f[x] = d[x] + e[x];
          }
          #pragma omp barrier
          #pragma omp target teams distribute
          for (int x = 0; x < 1024; ++x){
              is_host = omp_is_initial_device();
              g[x] = c[x] + f[x];
          }
      }
  }


  for (int x = 0; x < 1024; ++x){
      OMPVV_TEST_AND_SET(errors, (c[x] != 3 * x));
      OMPVV_TEST_AND_SET(errors, (f[x] != 7 * x));
      OMPVV_TEST_AND_SET(errors, (g[x] != 10 * x));
  }


  if (!errors) {
    OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  } else if (!is_host) {
    OMPVV_ERROR("Test failed on device with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  } else if (is_host) {
    OMPVV_ERROR("Test failed on host with offloading %s.", (isOffloading ? "enabled" : "disabled"));
  }

  OMPVV_REPORT_AND_RETURN(errors);
}
