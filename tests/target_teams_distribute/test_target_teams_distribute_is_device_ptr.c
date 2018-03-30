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
  int *c = (int *)omp_target_alloc(1024 * sizeof(int), omp_get_default_device());
  int errors = 0;
  if (!c){
      OMPVV_WARNING("Test was unable to allocate memory on device.  Test could not procede.");
      OMPVV_REPORT_AND_RETURN(errors);
  }
  else{
      int devtest = 1;
      #pragma omp target enter data map(to: devtest)
      #pragma omp target
      {
          devtest = 0;
      }
      // a and b array initialization
      for (int x = 0; x < 1024; ++x) {
          a[x] = 1;
          b[x] = x;
      }

      #pragma omp target data map(tofrom: a[0:1024]) map(to: b[0:1024])
      {
          #pragma omp target teams distribute is_device_ptr(c)
          for (int x = 0; x < 1024; ++x){
              c[x] = b[x] * b[x];
              a[x] += c[x] + b[x];
          }
      }

      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors, (a[x] != 1 + b[x] + b[x] * b[x]));
      }


      if (!errors) {
        OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
      } else if (devtest == 1) {
        OMPVV_ERROR("Test failed on device with offloading %s.", (isOffloading ? "enabled" : "disabled"));
      } else if (devtest == 0) {
        OMPVV_ERROR("Test failed on host with offloading %s.", (isOffloading ? "enabled" : "disabled"));
      }

      OMPVV_REPORT_AND_RETURN(errors);
  }
}
