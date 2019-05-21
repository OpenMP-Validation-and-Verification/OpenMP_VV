//===--- test_target_teams_distribute_device.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the device clause to indicate which device should execute the
// given target regions.  The test uses the separate device data environments to
// ensure that operations are executed on the specified device.  If only one device
// is available, the test issues a warning.
//
// By having a separate initialization of the same array on each device at the
// same time, if all operations were occuring on the same device, we would expect
// the same results from each device and it wouldn't be able to give proper answers
// for each initialization.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define ARRAY_SIZE 1024

int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);

  int a[ARRAY_SIZE];
  int b[ARRAY_SIZE];
  int num_devices = omp_get_num_devices();
  int num_teams[num_devices];
  int errors[num_devices];
  int sum_errors = 0;

  OMPVV_INFOMSG("running tests on %d devices", num_devices);

  for (int x = 0; x < ARRAY_SIZE; ++x) {
      a[x] = 1;
      b[x] = x;
  }
  for (int x = 0; x < num_devices; ++x){
      num_teams[x] = 0;
      errors[x] = 0;
  }

  for (int dev = 0; dev < num_devices; ++dev){
      #pragma omp target enter data map(to: a[0:ARRAY_SIZE], b[0:ARRAY_SIZE], num_teams[dev]) device(dev)
  }

  for (int dev = 0; dev < num_devices; ++dev){
      #pragma omp target teams distribute map(alloc: a[0:ARRAY_SIZE], b[0:ARRAY_SIZE], num_teams[dev]) device(dev)
      for (int x = 0; x < ARRAY_SIZE; ++x){
          num_teams[dev] = omp_get_num_teams();
          a[x] += b[x] + dev;
      }
  }

  for (int dev = 0; dev < num_devices; ++dev){
      #pragma omp target exit data map(from: a[0:ARRAY_SIZE], num_teams[dev]) map(delete: b[0:ARRAY_SIZE]) device(dev)
      for (int x = 0; x < ARRAY_SIZE; ++x){
          OMPVV_TEST_AND_SET_VERBOSE(errors[dev], a[x] != 1 + dev + b[x]);
          if (a[x] != 1 + dev + b[x]){
              break;
          }
      }
  }

  for (int x = 0; x < num_devices; ++x){
      sum_errors += errors[x];
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
