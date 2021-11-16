//===--- test_target_defaultmap_default.c --------------------------------------------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test checks behavior of the defaultmap clause when the specified implicit-behavior  
//  is present. The variable-categories avaiable for defaultmap are scalar, aggregate, and pointer.
//  If implicit-behavior is present, each variable referenced in the construct in the category specified
//  by variable-category is treated as if it had been listed in a map clause wih the map-type of alloc
//  and map-type-modifier of present.
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

   int scalar_var; // scalar 
   int A[N]; // aggregate 
   struct test_struct new_struct; // aggregate
   int *ptr; // scalar, pointer 
   errors = 0;

   scalar_var = 1; 
   A[0] = 0; A[50] = 50;
   new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10;
   ptr = &A[0]; 
   ptr[50] = 50; ptr[51] = 51;
   
   #pragma omp target enter data map(to: scalar_var, A, new_struct)
   
   #pragma omp target map(tofrom: errors) defaultmap(present) map(alloc,present: scalar_Var)
   {     
      if(scalar_var != 1){errors++;}
      if(A[0] != 0){errors++;}
      if(A[50] != 50 || A[51] != 51){errors++;}
      if(new_struct.s != 10){errors++;}
      if(new_struct.S[0] != 10 || new_struct.S[1] != 10){errors++;}
      
      scalar_var = 7; 
      A[0] = 70; A[50] = 150;
      new_struct.s = 110; new_struct.S[0] = 110; new_struct.S[1] = 110;
      ptr = &A[0]; 
      ptr[50] = 150; ptr[51] = 151;
   }
   
  for (i = 0; i < N; i++) {
      OMPVV_TEST_AND_SET(errors, scalar_var == 7);
      OMPVV_TEST_AND_SET(errors, A[0] == 70 || A[50] == 150 || A[51] = 151);
      OMPVV_TEST_AND_SET(errors, new_struct.s == 110 || new_struct.S[0] = 110 || new_struct.S[1] = 110);
  }
      
  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_present() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}           
