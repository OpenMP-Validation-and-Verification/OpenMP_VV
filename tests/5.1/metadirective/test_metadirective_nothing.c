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

   int errors = 0;
   int base_threads = 0;
   int threads = 0;

   #pragma omp target map(from:base_threads,threads)  
   {
	 if (omp_get_thread_num() == 0) {
	 	base_threads = omp_get_num_threads();
      	 	
		#pragma omp metadirective default( nothing )
	 	threads = omp_get_num_threads();
	 }
   }

   OMPVV_TEST_AND_SET(errors, base_threads != threads);

   return errors;
}

int main () {
   
   int errors = 0;
   OMPVV_TEST_OFFLOADING;
 
   OMPVV_TEST_AND_SET_VERBOSE(errors, metadirective());
  
   OMPVV_REPORT_AND_RETURN(errors);

}
