//===--------------------- test_metadirective_nothing.c ---------------------===//
//
// OpenMP API Version 5.1 Nov 2020
// 
// Test for nothing directive within metadirectives. Runs a variety of
// metadirectives that check if the nothing directive is properly rendered.
// Primarily tests based on the fact that no matter what 'when' clause is 
// rendered it should result in nothing, and thus no additional pragma should
// be created. Thus, the threads should remain unchanged through this process
// and the compiler should handle it properly.
//
////===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int metadirectiveOffload() {
   int errors = 0;
   int scalar = 0;
   int A[N];
   for (i = 0; i < N; i++) {
      A[i] = 0;
   }

   #pragma omp metadirective \
      when( device={kind(nohost)}: nothing ) \
      when( device={arch("nvptx")}: nothing) \
      when( device={arch("amd")}: nothing ) \
      default( target map(to:from scalar, A) )
      {
         scalar = 10;
         for (i = 0; i < N; i++) {
            A[i] = i + 2;
         }
      }



   for (i = 0; i < N; i++) {
	if (A[i] != 0) {
		errors++;
	}
   }


   OMPVV_TEST_AND_SET_VERBOSE(errors)

}

int metadirective() {

   int errors = 0;
   int base_threads = 0;
   int threads = 0;

   #pragma omp target map(from:base_threads,threads)  
   {
	 	base_threads = omp_get_num_threads();
      	 	
		#pragma omp metadirective \
		   when( device={kind(nohost)}: nothing ) \
		   when( device={arch("nvptx")}: nothing ) \
		   default( nothing )
		#pragma omp metadirective \
		   when( implementation={vendor(nvidia)}: nothing) \
		   when( device={kind(nohost)}: nothing) \
		   default( nothing )
		#pragma omp metadirective \
		   when( implementation={vendor(amd)}: nothing) \
		   when( device={kind(nohost)}: nothing) \
		   default( nothing )
		#pragma omp metadirective \
		   when( implementation={arch("amd")}: nothing) \
		   when( device={kind(nohost)}: nothing) \
		   default( nothing )	
		#pragma omp metadirective \
		   when( construct={target}: nothing) \
		   default( nothing ) 

		threads = omp_get_num_threads();
   }

   OMPVV_TEST_AND_SET(errors, base_threads != threads);

   return errors;
}

int main () {
   
   int errors = 0;
   OMPVV_TEST_OFFLOADING;

   metadirectiveOffload();
  
   OMPVV_REPORT_AND_RETURN(errors);

}
