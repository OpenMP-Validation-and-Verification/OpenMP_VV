//-------------test_scope_construct.c---------------------//
//
//OpenMP API Version 5.1 Aug 2021
//
//Tests the behavior of the scope construct with no clauses
//specified.
//--------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 64

int test_scope() {
  int errors = 0;
  int total = 0;
  #pragma omp parallel shared(total) target(tofrom: total)
	{
		#pragma omp scope
		{
      #pragma omp for 
      for (int i=0; i < N; ++i){
        ++total;
      }
    }
  }
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, total != N);
  return errors;
}
int main() {
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_scope() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
