#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

#define N 1024

int test_map_present() {

   struct test_struct {
     int s;
     int S[N];
   };

   int errors = 0;
   int i; 
   int scalar; // scalar
   int A[N]; // aggregate
   struct test_struct new_struct; // aggregate
   int *ptr; // scalar, pointer


   for(i = 0; i<N; i++){
	A[i] = i;
   }

   //scalar = 1;
   /*
   A[0] = 0; A[50] = 50;
   new_struct.s = 10; new_struct.S[0] = 10; new_struct.S[1] = 10;
   ptr = &A[0];
   ptr[50] = 50; ptr[51] = 51;
   */


#pragma omp target enter data map (to :A)
#pragma omp target map(from: A) map(present, to: A) map(tofrom: errors)
{
   for (i=0; i<N; i++) {
      if (A[i] != i) {
	 errors++;
      }
      A[i] = 2*i;
   }
}
   for(i=0; i<N; i++){
	if( i== 1023)   
	  printf("%d\n", A[i]);
   }

   return errors;

}
   int main() {
   int errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_map_present() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}              
