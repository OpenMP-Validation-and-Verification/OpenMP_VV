#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512
#define ARRAY_SIZE 1024

// Test for OpenMP 4.5 target data with if
int main() {
  OMPVV_TEST_OFFLOADING;
  OMPVV_INFOMSG("test target_teams_distribute_is_device_ptr");

  int a[ARRAY_SIZE];
  int b[ARRAY_SIZE];
  int *c = (int *)omp_target_alloc(ARRAY_SIZE * sizeof(int), omp_get_default_device());
  int errors = 0;
  if (!c){
      OMPVV_WARNING("Test was unable to allocate memory on device.  Test could not procede.");
      OMPVV_REPORT_AND_RETURN(errors);
  } 
  else {
      // a and b array initialization
      for (int x = 0; x < ARRAY_SIZE; ++x) {
          a[x] = 1;
          b[x] = x;
      }

      #pragma omp target data map(tofrom: a[0:ARRAY_SIZE]) map(to: b[0:ARRAY_SIZE])
      {
          #pragma omp target teams distribute is_device_ptr(c)
          for (int x = 0; x < ARRAY_SIZE; ++x){
              c[x] = b[x] * b[x];
              a[x] += c[x] + b[x];
          }
      }

      for (int x = 0; x < ARRAY_SIZE; ++x){
          OMPVV_TEST_AND_SET(errors, (a[x] != 1 + b[x] + b[x] * b[x]));
      }

      OMPVV_REPORT_AND_RETURN(errors);
  }
}
