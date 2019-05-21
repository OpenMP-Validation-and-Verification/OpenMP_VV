//===--- test_target_teams_distribute_num_threads.c--------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// This test uses the num_threads clause on a target teams distribute directive to
// indicate a requested number of threads to execute the teams distribute region.
// The specifications indicate that the number of threads that are given can be any
// number that is equal to or less than the indicated value.  We first run a
// target teams distribute region without the clause to see what the default
// number of threads is, and then we use a value that is less than that in the
// test of the num_threads clause.  If the region is run with more threads than
// indicated, the test errors.  If the region is run with less threadsthan
// indicated, the test issues a warning since it is known that the device can
// run with more threads than was actually given.
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
  int default_threads = 0;
  int num_threads = 0;
  int errors = 0;

  #pragma omp target data map(tofrom: default_threads)
  {
      #pragma omp target teams distribute map(alloc: default_threads)
      for (int x = 0; x < 1024; ++x){
          default_threads = omp_get_thread_limit();
      }
  }

  if (default_threads == 1){
      OMPVV_WARNING("Test operated with one thread.  Testing of thread_limit clause cannot be done.");
  }
  else if(default_threads <= 0){
      OMPVV_ERROR("Test returned thread_limit == 0.  Maybe omp_get_thread_limit() is not returning correct number of teams.");
      errors = 1;
  }
  else{
      #pragma omp target data map(from: num_threads)
      {
          #pragma omp target teams distribute thread_limit(default_threads - 1) map(alloc: num_threads)
          for (int x = 0; x < 1024; ++x){
              num_threads = omp_get_thread_limit();
          }
      }
      if (num_threads > default_threads - 1){
          errors += 1;
          OMPVV_ERROR("Test ran on more threads than requested.");
          return errors;
      }
      else if (num_threads < default_threads - 1){
          OMPVV_WARNING("Test ran on less threads than requested.  Still spec-conformant.");
      }
      else{
          OMPVV_INFOMSG("Test passed with offloading %s", (isOffloading ? "enabled" : "disabled"));
      }
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
