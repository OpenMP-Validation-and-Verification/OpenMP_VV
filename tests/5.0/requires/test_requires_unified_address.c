//===---test_requires_unified_address.c -------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
// 
// This test checks for support of unified_address clause on the requires
// directive.
//
/////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

#pragma omp requires unified_address

int unified_address() {

   int errors = 0;
   int i;
   int * mem_ptr = (int *)malloc(N * sizeof(int));

   OMPVV_ERROR_IF(mem_ptr == NULL, "Memory was not properly allocated");
   
   #pragma omp target map(to: mem_ptr)
   {
      for (i = 0; i < N; i++) {
         mem_ptr[i] = i + 1;
      }
   }
   
   for (i = 0; i < N; i++) {
      if(mem_ptr[i] != i + 1) {
         errors++;
      }  
   }
   
   return errors;
}

int main() {
  
  int errors = 0;

  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, unified_address());
  OMPVV_REPORT_AND_RETURN(errors);

}  
