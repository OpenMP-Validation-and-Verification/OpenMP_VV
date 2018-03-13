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
  int num_devices = omp_get_num_devices();
  int num_teams[num_devices];
  int errors[num_devices];
  int sum_errors = 0;

  // a and b array initialization
  for (int x = 0; x < 1024; ++x) {
      a[x] = 1;
      b[x] = x;
  }
  for (int x = 0; x < num_devices; ++x){
      num_teams[x] = 0;
      errors[x] = 0;
  }

  for (int dev = 0; dev < num_devices; ++dev){
      #pragma omp target enter data map(to: a[0:1024], b[0:1024], num_teams[dev]) device(dev)
  }

  for (int dev = 0; dev < num_devices; ++dev){
      #pragma omp target teams distribute map(alloc: a[0:1024], b[0:1024], num_teams[dev]) device(dev)
      for (int x = 0; x < 1024; ++x){
          num_teams[dev] = omp_get_num_teams();
          a[x] += b[x] + dev;
      }
  }

  for (int dev = 0; dev < num_devices; ++dev){
      #pragma omp target exit data map(from: a[0:1024], num_teams[dev]) map(delete: b[0:1024]) device(dev)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TEST_AND_SET(errors[dev], a[x] != 1 + dev + b[x]);
      }
  }

  for (int x = 0; x < num_devices; ++x){
      sum_errors += errors[x];
  }

  if (!sum_errors){
      OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  }
  else{
      OMPVV_ERROR("Test failed with offloading %s", (isOffloading ? "enabled" : "disabled"));
  }
  for (int dev = 0; dev < num_devices; ++dev){
      if (!errors[dev] && num_teams[dev] == 1){
          OMPVV_WARNING("Test operated with one team. Parallelism of teams distribute can't be guarnunteed.");
      }
      else if (errors[dev]){
          OMPVV_ERROR("Test failed with device %d", dev);
      }
  }
  OMPVV_REPORT_AND_RETURN(sum_errors);
}
