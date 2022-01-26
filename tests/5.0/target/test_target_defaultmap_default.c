//===--- test_target_defaultmap_default.c --------------------------------------------------------------===//
//
//  OpenMP API Version 5.0 Nov 2018
//
//  This test checks behavior of the defaultmap clause when the specified implicit-behavior  
//  is default. The variable-categories avaiable for defaultmap are scalar, aggregate, and pointer.
//  When no mapping is specified, the implicit-behavior of these variable-categories are
//  firstprivate, tofrom, and firstprivate, respectively. This test specifies the default implicit-behavior 
//  for the three variable-categories and expects the same behavior as when there is no
//  explicit mapping.
//
////===-------------------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors, i;

int test_defaultmap_with_default() {
   
   struct test_struct {
     int s; 
     int S[N]; 
   }; 

   int scalar; // scalar 
   int A[N]; // aggregate 
   struct test_struct new_struct; // aggregate
   int *ptr; // scalar, pointer 

   scalar = 1; 
   new_struct.s = 0;

   for (i = 0; i < N; i++) {
      A[i] = 0;
      new_struct.S[i] = 0;
   }

   #pragma omp target defaultmap(default) 
   {     
      scalar = 17;    // scalar firstprivate, value not returned
      A[0] = 5; A[1] = 5; // aggregate array, default is tofrom
      new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10; // aggregate structure, default is tofrom
      ptr = &A[0]; // Pointer, default is firstprivate
      ptr[50] = 50; ptr[51] = 51;
   }

   OMPVV_TEST_AND_SET_VERBOSE(errors, scalar != 1);
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[0] != 5 || A[1] != 5);
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[50] != 50 || A[51] != 51);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.s != 10);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.S[0] != 10);

  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_with_default() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}           
