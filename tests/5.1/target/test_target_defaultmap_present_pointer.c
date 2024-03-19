//===--- test_target_defaultmap_present_pointer.c ---------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test checks behavior of the defaultmap clause when the specified 
//  implicit-behavior is present. The variable-categories available for defaultmap
//  are scalar, aggregate, and pointer. If implicit-behavior is present, each 
//  variable referenced in the construct in the category specified by 
//  variable-category is treated as if it had been listed in a map clause wih the
//  map-type of alloc and map-type-modifier of present.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_defaultmap_present_pointer() {
   
   int errors = 0;
   int i;
   
   int *ptr;
   int A[N];
   
   for (i = 0; i < N; i++) {
      A[i] = i;
   }
   
   
   #pragma omp target enter data map(to: ptr)
   #pragma omp target map(tofrom: errors) defaultmap(present: pointer)
   {  
      ptr = &A[0];
      for (i = 0; i < N; i++) {
         if (ptr[i] != i) {
           errors++;
         }
         ptr[i] = 2+i;
      }
   }
   
   OMPVV_ERROR_IF(errors > 0, "Values were not mapped to the device properly");
   
   for (i = 0; i < N; i++) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, A[i] != 2+i);
   }

   return errors;
}

int main() {
   int errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_present_pointer() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}
