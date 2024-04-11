//===---test_target_requires_atomic_default_mem_order_relaxed.c--------------===//
//
// OpenMP API Version 5.0 Nov 2018
// 
// This test checks for support of the atomic_default_mem_order clause on the 
// requires directive. This clause determines the default memory behavior for
// atomic constructs. These behaviors are seq_cst, acq_rel, and relaxed.
// This test checks for relaxed as the memory-order-clause.
//
// Adapted from 5.0 OpenMP example acquire_release.3.c
////===----------------------------------------------------------------------===//

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

#pragma omp requires atomic_default_mem_order(relaxed)

int test_target_requires_atomic_relaxed() {
  OMPVV_INFOMSG("test_target_requires_atomic_relaxed");

  int x = 0, y = 0;
  int errors = 0, tmp = 0;

#pragma omp target parallel num_threads(2) map(tofrom: x, y, errors) map(to: tmp)
   {
      int thrd = omp_get_thread_num();
       if (thrd == 0) {
          x = 10;
          #pragma omp flush
          #pragma omp atomic write 
          y = 1;
       } else {
          tmp = 0;
       }

       // Instead of a else as in the original test, a separated if was included      
       // Given that the Device can be executed on SPMD, it is
       // possible that the else can be executed first by all threads
       // generating a deadlock on the while
       // furthermore, it also necesary to create an else, with
       // a shared variable to prevent the optimizer to marge the
       // if and have the same problem than before
       // therefore a separated if is geenrated for the code
       // and this way it is possible to guarantee that there is
       // no deadlock on the while
       if (thrd == 1) {
          while (tmp == 0) {
            #pragma omp atomic read 
            tmp = y;
          }
          #pragma omp flush
          OMPVV_TEST_AND_SET(errors, x != 10);
       }
   }
   OMPVV_ERROR_IF(errors > 0, "Requires atomic_default_mem_order(relaxed) test failed");
   return errors;
}


int main() {

  int errors = 0;

  OMPVV_TEST_OFFLOADING;

  OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_requires_atomic_relaxed());

  OMPVV_REPORT_AND_RETURN(errors);
}
