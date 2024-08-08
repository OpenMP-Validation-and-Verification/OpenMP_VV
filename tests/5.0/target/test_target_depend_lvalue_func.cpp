//===--- test_target_depend_lvalue_func.cpp ----------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test checks the depend clause on task generating constructs are extended to
// allow any lvalue such as functions that return references.
//
////===----------------------------------------------------------------------===//

#include <omp.h>
#include <iostream>
#include <cstdlib>
#include "ompvv.h"

#pragma omp declare target
// Global variable var
int var;

// Function to return reference to var
int& ref() {
	return var;
}
#pragma omp end declare target
int test_task_depend_lvalue_func() {
	int errors = 0;
	int value = 0;
	var = 0;
	#pragma omp target parallel map(tofrom: value)
	{
		#pragma omp single
		{	
			// Use the ref() function in the
			// depend clause and update var's value
			#pragma omp task depend(out: ref())
			{
				ref() = 1;
			}
			
			// Set value equal to var using the
			// ref() function
			#pragma omp task depend(in: ref())
			{
				value = ref();	
			}
			#pragma omp taskwait
		}
	}
	
	//Check if value was updated to 1
	OMPVV_TEST_AND_SET(errors, value != 1);
	return errors;
}

int main() {
	OMPVV_TEST_OFFLOADING;
	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_depend_lvalue_func() != 0);

	OMPVV_REPORT_AND_RETURN(errors);

	return errors;
}

