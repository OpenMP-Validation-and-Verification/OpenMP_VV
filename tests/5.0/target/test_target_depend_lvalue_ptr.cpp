//===--- test_target_depend_lvalue_ptr.cpp -----------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the depend clause on task generating constructs are extended to
// allow any lvalue such as dereferenced pointers.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <iostream>
#include <cstdlib>
#include "ompvv.h"

int test_task_depend_lvalue_ptr() {
	int errors = 0;
	int *ptr = new int;
	int value = 0;

	// Initialize the pointer to -1
	*ptr = -1;

	#pragma omp target parallel map(tofrom: *ptr, value)
	{
		#pragma omp single
		{
			#pragma omp task depend(out: *ptr)
			{
				*ptr = 1;
			}

			#pragma omp task depend(in: *ptr)
			{
				value = *ptr;
			}
			#pragma omp taskwait
		}
	}

	delete ptr;
	
	OMPVV_TEST_AND_SET(errors, value != 1);
	
	return errors;
}

int main() {
	OMPVV_TEST_OFFLOADING;
	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_depend_lvalue_ptr() != 0);

	OMPVV_REPORT_AND_RETURN(errors);

	return errors;
}

