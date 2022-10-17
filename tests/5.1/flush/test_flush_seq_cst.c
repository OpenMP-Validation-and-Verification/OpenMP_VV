//===---- test_flush_seq_cst.c ----------------------------------------------===//
// 
// OpenMP API Version 5.1 Nov 2020
// 
// This is a test of the flush directive with no memory-order-clause specified.
// Additionally, atomic constructs are used alongside the explicit flush directives
// to specify memory ordering amongst the two threads.
//
// Based on OpenMP 5.0 Example aquire_release.3.c
//
//===------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

int errors = 0;

int main() {

   int x = 0, y = 0;
   #pragma omp parallel num_threads(2)
   {
      int thrd = omp_get_thread_num();
      if (thrd == 0) {
         x = 10;
         #pragma omp flush seq_cst
         #pragma omp atomic write
         y = 1;
      } else {
         int tmp = 0;
         while (tmp == 0) {
            #pragma omp atomic read
	    tmp = y;
         }
         #pragma omp flush seq_cst
         OMPVV_TEST_AND_SET_VERBOSE(errors, x != 10);
      }
  }
  OMPVV_REPORT_AND_RETURN(errors);
}
