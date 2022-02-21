//===--------------------- test_metadirective_nothing.c ---------------------===//
//
// OpenMP API Version 5.1 Nov 2020
// 
// Test for nothing directive within metadirectives. Runs a variety of
// metadirectives that check if the nothing directive is properly rendered.
//
////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int metadirective() {
   
   int v1[N], v2[N], v3[N];

   int errors = 0;
   int teams = 0;

   for(int i=0; i<N; i++) { 
      v1[i] = (i+1); 
      v2[i] = -(i+1); 
   }

   #pragma omp target map(to:v1,v2) map(from:v3,teams)  
   {
      #pragma omp metadirective \
                   when(   device={arch("nvptx")}: teams distribute parallel for) \
                   default(                        nothing)

	 teams = 
         for (int i = 0; i < N; i++) {
	    #pragma omp atomic write
            v3[i] = v1[i] * v2[i];
         }
   }
   
   for (int i = 0; i < N; i++) {
      OMPVV_TEST_AND_SET_VERBOSE(errors, v3[i] != v1[i] * v2[i]);
   }

   OMPVV_TEST_AND_SET(errors, test != 1);

   return errors;
}

int main () {
   
   int errors = 0;
   OMPVV_TEST_OFFLOADING;
 
   OMPVV_TEST_AND_SET_VERBOSE(errors, metadirective());
  
   OMPVV_REPORT_AND_RETURN(errors);

}
