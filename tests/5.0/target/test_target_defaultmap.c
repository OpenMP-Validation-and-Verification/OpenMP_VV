//===--- test_target_defaultmap.c -----------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
//
////===---------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024


int test_default_map() {

   struct test_struct {
     int s; 
     int S[N]; 
   } 

   int scalar; // scalar int variable -> scalar
   int A[N]; // aggregate variable -> array
   test_struct new_struct; // aggregate variable -> structure
   int *ptr; // scalar, pointer variable -> pointer

   // initialize everything
   scalar = 1; 
   A[0] = 0; A[1] = 0;
   new_struct.s = 0;
   new_struct.S[0] = 0;
   new_struct.S[1] = 0;  


   // Set to standard default mappings explicity using defaultmap
   #pragma omp target defaultmap(firstprivate: scalar) \
                      defaultmap(tofrom:    aggregate) \
                      defaultmap(default:     pointer) \
                      map(ptr[:N])
   {     
      scalar = 17;    // Scalar firstprivate, value not returned

      A[0] = 5; A[1] = 5; // Aggregate array, default is tofrom

      new_struct.s = 10;  // Aggregate structure, default is tofrom

      new_struct.S[0] = 10; new_struct.S[1] = 10; // Aggregate structure, default is tofrom
   
      ptr = &A[0]; // Pointer, default is private
      ptr[0] = 50; ptr[1] = 50;
   }

   OMPVV_TEST_AND_SET(errors, scalar != 1);
   OMPVV_TEST_AND_SET(errors, A[0] != 50);
   OMPVV_TEST_AND_SET(errors, new_struct.s ! = 10);
   OMPVV_TEST_AND_SET(errors, new_struct.S[0] != 10);

}


int main() {

   OMPVV_TEST_OFFLOADING;
   int errors = 0;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_default_map() != 0);
   OMPVV_REPORT_AND_RETURN(errors);

}           
