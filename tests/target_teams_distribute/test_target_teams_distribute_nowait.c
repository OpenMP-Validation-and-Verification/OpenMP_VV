//===--- test_target_teams_distribute_nowait.c-------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the nowait clause on a target teams distribute directive and
// uses a barrier to resyncronize the target regions.  Since we can't be sure
// that operations will be asyncronous, we can not test to make sure that
// the regions are executed asynchronously.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define ARRAY_SIZE 1024
#define ITERATIONS 1024
int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[ARRAY_SIZE];
  int b[ARRAY_SIZE];
  int c[ARRAY_SIZE];
  int d[ARRAY_SIZE];
  int e[ARRAY_SIZE];
  int f[ARRAY_SIZE];
  int errors = 0;
  int race_condition_found = 0;

  for (int y = 0; y < ITERATIONS; ++y){
      for (int x = 0; x < ARRAY_SIZE; ++x) {
          a[x] = x + y;
          b[x] = 2 * x + y;
          c[x] = 0;
          d[x] = 3 * x + y;
          e[x] = 4 * x + y;
          f[x] = 0;
      }

      #pragma omp target data map(to: a[0:ARRAY_SIZE], b[0:ARRAY_SIZE], d[0:ARRAY_SIZE], e[0:ARRAY_SIZE]) map(from: c[0:ARRAY_SIZE], f[0:ARRAY_SIZE]) 
      {
          #pragma omp target teams distribute nowait map(alloc: a[0:ARRAY_SIZE], b[0:ARRAY_SIZE], c[0:ARRAY_SIZE])
          for (int x = 0; x < ARRAY_SIZE; ++x){
              c[x] = a[x] + b[x];
          }
          #pragma omp target teams distribute nowait map(alloc: c[0:ARRAY_SIZE], d[0:ARRAY_SIZE], e[0:ARRAY_SIZE], f[0:ARRAY_SIZE])
          for (int x = 0; x < ARRAY_SIZE; ++x){
              f[x] = c[x] + d[x] + e[x];
          }
#pragma omp target
           {
            race_condition_found = 0;
           }
      }

      for (int x = 0; x < ARRAY_SIZE; ++x){
          OMPVV_TEST_AND_SET_VERBOSE(errors, c[x] != 3 * x + 2 * y);
          if (f[x] != 10 * x + 4 * y){
              race_condition_found = 1;
          }
      }
  }

  OMPVV_WARNING_IF(race_condition_found == 0, "Could not show that nowait was operating on target teams distribute construct");

  OMPVV_REPORT_AND_RETURN(errors);
}
