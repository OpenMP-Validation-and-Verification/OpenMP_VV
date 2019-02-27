//===--- test_target_teams_distribute_lastprivate.c--------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the lastprivate clause to indicate that the privatized value
// that is passed as the parameter should also be returned with the value that
// results from the thread that runs the last iteration of the for loop in the
// target teams distribute directive.  The clause can be used with both scalar
// and array data types and both situations are tested.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define SIZE_THRESHOLD 512

int main() {
  int isOffloading = 0;
  OMPVV_TEST_AND_SET_OFFLOADING(isOffloading);
  int a[1024];
  int b[1024];
  int c[1024];
  int privatized = 0;
  int privatized_array[2];
  int errors = 0;

  for (int x = 0; x < 1024; ++x){
      a[x] = 1;
      b[x] = x;
      c[x] = 0;
  }




  #pragma omp target data map(to: a[0:1024], b[0:1024]) map(tofrom: c[0:1024])
  {
      #pragma omp target teams distribute lastprivate(privatized) map(to: a[0:1024], b[0:1024], c[0:1024]) map(from: privatized)
      for (int x = 0; x < 1024; ++x){
          privatized = a[x] - b[x];
          c[x] = privatized + b[x];
      }
  }

  for (int x = 0; x < 1024; ++x){
      OMPVV_TEST_AND_SET_VERBOSE(errors, c[x] - a[x] != 0);
      if (c[x] - a[x] != 0){
          break;
      }
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, privatized != a[1023] - b[1023]);

  for (int x = 0; x < 1024; ++x){
      a[x] = 1;
      b[x] = x;
      c[x] = x % 10;
  }

  #pragma omp target data map(to: a[0:1024], b[0:1024], c[0:1024]) map(tofrom: privatized_array[0:2])
  {
      #pragma omp target teams distribute lastprivate(privatized_array) map(alloc: a[0:1024], b[0:1024], c[0:1024])
      for (int x = 0; x < 1024; ++x){
          privatized_array[0] = a[x] + b[x] + c[x];
          privatized_array[1] = (a[x] + b[x]) * c[x];
      }
  }

  OMPVV_TEST_AND_SET_VERBOSE(errors, privatized_array[0] != (a[1023] + b[1023] + c[1023]));
  OMPVV_TEST_AND_SET_VERBOSE(errors, privatized_array[1] != ((a[1023] + b[1023]) * c[1023]));
  OMPVV_REPORT_AND_RETURN(errors);
}
