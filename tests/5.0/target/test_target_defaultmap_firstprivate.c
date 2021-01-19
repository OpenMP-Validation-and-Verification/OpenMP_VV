//===--- test_target_defaultmap_firstprivate.c --------------------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks behavior of the defaultmap clause when the specified implicit-behavior  
// is firstprivate. The variable-categories avaiable for defaultmap are scalar, aggregate, and pointer.
// All variables (pointer, aggregate, scalar) will be mapped with firstprivate behavior, thus, expected 
// behavior is that all list items will have their original value.
//
//
////===-----------------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors;
int i;

int test_defaultmap_with_firstprivate() {

   struct test_struct {
     int s;
     int S[N];
   };

   int scalar; // scalar 
   int A[N]; // aggregate 
   struct test_struct new_struct; // aggregate variable -> structure
   int *ptr; // scalar, pointer variable -> pointer

   // initialize everything
   scalar = 1;
   new_struct.s = 0;

   for (i = 0; i < N; i++) {
      A[i] = 0;
      new_struct.S[i] = 0;
   }

   #pragma omp target defaultmap(firstprivate) map(from:ptr)
   {
      scalar = 17;    
      A[0] = 5; A[1] = 5;
      new_struct.s = 10;
      new_struct.S[0] = 10; new_struct.S[1] = 10; 
      ptr = &A[0]; 
      ptr[50] = 50; ptr[51] = 51;
   }
   OMPVV_TEST_AND_SET_VERBOSE(errors, scalar != 1);
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[0] != 0 || A[1] != 0);
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[50] != 0 || A[51] != 0);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.s != 0);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.S[0] != 0);

  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_with_firstprivate() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}            
