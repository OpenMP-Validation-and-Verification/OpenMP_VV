//===--- test_target_defaultmap_to_from_tofrom.c ----------------------------===//
//
//  OpenMP API Version 5.0 Nov 2018
//
//  This test checks behavior of the defaultmap clause when the specified 
//  implicit-behavior is to, from, and tofrom. The variable-categories 
//  avaiable for defaultmap are scalar, aggregate, 
//  and pointer.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int errors;
int i;

int test_defaultmap_with_to() {

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

   #pragma omp target defaultmap(to) //map(tofrom: scalar, A, new_struct, ptr) 
   {
      scalar = 17;    // Scalar firstprivate, value not returned
      A[0] = 5; A[1] = 5; // Aggregate array, default is tofrom
      new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10; // Aggregate structure, default is tofrom
      ptr = &A[0]; ptr[50] = 70; ptr[51] = 71; // Pointer, default is privatei
   }

   OMPVV_TEST_AND_SET_VERBOSE(errors, scalar != 1); 
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[0] != 0 || A[1] != 0); 
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[50] != 0 || A[51] != 0);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.s != 0 || new_struct.S[0] != 0 || new_struct.S[1] != 0);
   
   return errors;
}

int test_defaultmap_with_from() {

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

   #pragma omp target defaultmap(from) //map(tofrom: scalar, A, new_struct, ptr) 
   {
      scalar = 17;    // Scalar firstprivate, value not returned
      A[0] = 5; A[1] = 5; // Aggregate array, default is tofrom
      new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10; // Aggregate structure, default is tofrom
      ptr = &A[0]; ptr[50] = 50; ptr[51] = 51; // Pointer, default is private
   }

   OMPVV_TEST_AND_SET_VERBOSE(errors, scalar != 17); 
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[0] != 5 || A[1] != 5); 
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[50] != 50 || A[51] != 51);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.s != 10 || new_struct.S[0] != 10 || new_struct.S[1] != 10);

   return errors;
}

int test_defaultmap_with_tofrom() {

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

   #pragma omp target defaultmap(tofrom) //map(tofrom: scalar, A, new_struct, ptr) 
   {
      scalar = 17;    // Scalar firstprivate, value not returned
      A[0] = 5; A[1] = 5; // Aggregate array, default is tofrom
      new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10; // Aggregate structure, default is tofrom
      ptr = &A[0]; ptr[50] = 50; ptr[51] = 51; // Pointer, default is private
   }

   OMPVV_TEST_AND_SET_VERBOSE(errors, scalar != 17); 
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[0] != 5 || A[1] != 5); 
   OMPVV_TEST_AND_SET_VERBOSE(errors, A[50] != 50 || A[51] != 51);
   OMPVV_TEST_AND_SET_VERBOSE(errors, new_struct.s != 10 || new_struct.S[0] != 10 || new_struct.S[1] != 10);

   return errors;
}

int main() {

   errors = 0;

   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_with_to() != 0);
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_with_from() != 0);
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_defaultmap_with_tofrom() != 0);
   OMPVV_REPORT_AND_RETURN(errors);

}
