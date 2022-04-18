//===------ test_target_allocate.c --------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target directive with allocate clause.
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_target_allocate() { 

   int errors = 0;

   int x = 0;
   int host_result = 0, device_result = 0;

   for (int i = 0; i < N; i++) {
      host_result += i;
   }
   
   #pragma omp target allocate(omp_default_mem_alloc:x) firstprivate(x) map(from: device_result)
   {
      for (int i = 0; i < N; i++) {
         x += i;
      }
      device_result = x; 
  
   }
   
   OMPVV_TEST_AND_SET(errors, device_result != host_result);
   
   return errors; 
}

int main() {

   OMPVV_TEST_OFFLOADING;
 
   int errors = 0;

   OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_allocate() != 0);

   OMPVV_REPORT_AND_RETURN(errors);

}
