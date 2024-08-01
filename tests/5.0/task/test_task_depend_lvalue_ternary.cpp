//===--- test_task_depend_lvalue_ternary.cpp ---------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the depend clause on task generating constructs are extended to
// allow any lvalue such as ternary operators.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <iostream>
#include <cstdlib>
#include "ompvv.h"

int test_task_depend_ternary() {
	int errors = 0;
	int a = 1;
	int b = 0;
	int c = -1;
	int value = 0;

	#pragma omp target parallel map(to:a,b,c) map(from: value)
	{
		#pragma omp single
		{
			// Task should run first and set b = 1
			#pragma omp task depend(out: (a > b) ? b : c) shared(a,b)
			{
				if ( a > b) {
					b = 1;
				}
				else {
					c = 1;
				}
			}

			// Task should compute (a > b) ? b : c -> this will return
			// false. It should set value = c => -1
			#pragma omp task depend(in: (a > b) ? b : c) shared(a, b)
			{
				value = (a > b) ? b : c;
			}

			#pragma omp taskwait
		}
	}
	
	// Check to make sure value was correctly set to -1
	OMPVV_TEST_AND_SET(errors, value != -1);
	return errors;
}

int main() {
	OMPVV_TEST_OFFLOADING;
	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_depend_ternary() != 0);

	OMPVV_REPORT_AND_RETURN(errors);

	return errors;
}

