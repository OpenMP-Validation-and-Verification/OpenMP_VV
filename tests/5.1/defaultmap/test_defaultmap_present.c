//===--- test_target_defaultmap_default.c --------------------------------------------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test checks behavior of the defaultmap clause when the specified implicit-behavior  
//  is present. The variable-categories avaiable for defaultmap are scalar, aggregate, and pointer.
//  When no mapping is specified, all variables (pointer, aggregate, scalar) will be mapped with present
//  behavior, thus, expected behavior is that all list items are treated as if they had been listed in a
//  map clause with map-type alloc.
//
////===-------------------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors, i;

int test_defaultmap_present() {
   
   struct test_struct {
     int s; 
     int S[N]; 
   }; 

   int scalar; // scalar 
   int A[N]; // aggregate 
   struct test_struct new_struct; // aggregate
   int *ptr; // scalar, pointer 

   scalar = 1; 
   A[0] = 0; A[50] = 50;
   new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10;
   ptr = &A[0]; 
   ptr[50] = 50; ptr[51] = 51;

   #pragma omp target defaultmap(present) // Present means values should remain as if already declared in map as alloc
   {     
      OMPVV_TEST_AND_SET_VERBOSE(errors, scalar != 1);
      OMPVV_TEST_AND_SET_VERBOSE(errors, A[0] != 0 || A[50] != 50);
      OMPVV_TEST_AND_SET_VERBOSE(errors, A[50] != 50 || A[51] != 51);
      OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.s != 10);
      OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.S[0] != 10);
   }
  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_present() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}           
