//===--- test_motion_present.c --------------------------------------------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test checks behavior of the target update clause when the specified motion-modifier  
//  is present. The motion-clauses available for target update are to & from, and when
//  motion-modifier 'present' is used, all variables are mapped as if they had been listed
//  in a map clause with map-type 'alloc'.
//
////===-------------------------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors, i;

int test_motion_present() {
   
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

   #pragma omp target enter data map(alloc:scalar) map(alloc: A[N]) map(alloc: new_struct) //map(alloc: *ptr)
   #pragma omp target update to(present) // Present means values should remain as if already declared in map as alloc
   #pragma omp target map(tofrom: errors)
   {     
        if(scalar != 1){errors++;}
        if(A[0] != 0 || A[50] != 50){errors++;}
        if(A[50] != 50 || A[51] != 51){errors++;}
        if(new_struct.s != 10){errors++;}
        if(new_struct.S[0] != 10){errors++;}
   }

  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_motion_present() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}           
