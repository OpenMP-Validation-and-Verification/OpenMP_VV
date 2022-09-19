//===--- test_depend_inoutset.c ----------------------------===//
//
//  OpenMP API Version 5.1 Aug 2021
//
//  This test verifies the use of inoutset in depend clause.
//  Task T4 & T5 both rely on the use of c, and should run in any order.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int errors;

int depend_inoutset(){
    #pragma omp parallel
#pragma omp single
  {
#pragma omp task depend(out: c)
   c = 1; /* Task T1 */
 #pragma omp task depend(out: a)
   a = 2; /* Task T2 */
 #pragma omp task depend(out: b)
   b = 3; /* Task T3 */
 #pragma omp task depend(in: a) depend(inoutset: c)
   c += a; /* Task T4 */
 #pragma omp task depend(in: b) depend(inoutset: c)
   c += b; /* Task T5 */
 #pragma omp task depend(in: c)
   d = c; /* Task T6 */
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, d != 6);
  return errors;
}

int main() {
   errors = 0;
   OMPVV_TEST_OFFLOADING;
   OMPVV_TEST_AND_SET_VERBOSE(errors, test_depend_intouset() != 0);
   OMPVV_REPORT_AND_RETURN(errors);
}