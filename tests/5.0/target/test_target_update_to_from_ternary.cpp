//===----------------- test_target_update_to_from_lvalue_ternary.cpp --------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the update to and update from directives as well as the map clause  
// using ternary operators to generate lvalues.
//  
// 
//
//===----------------------------------------------------------------------===//
#include <omp.h>
#include <cstdlib>
#include <iostream>
#include "ompvv.h"

int test_pointer_with_ternary() {
	int errors = 0;
	int before_value;
	int after_value;
	int a = 1;
	int b = -1;
	int c = 0;

	// Copy data to the device using the ternary operator directly in the map clause
	#pragma omp target enter data map(to: (a > b) ? b : c)

	// Get the initial value from the device
	#pragma omp target map(from: before_value)
	{
		before_value = (a > b) ? b : c;
	}

 	b = 2;

	// Copy data from the host variable to the device variable using the ternary operator
	#pragma omp target update to((a > b) ? b : c)

	// Get the updated value from the device
	#pragma omp target map(from: after_value)
	{
		after_value = (a > b) ? b : c;
	}

	// Copy the final value from the device to the host
	#pragma omp target update from((a > b) ? b : c)

	// Verify the results
	if (before_value != -1) {
		errors++;
	}
	if (after_value != 0) { 
		errors++;
	}


	return errors;
}

int main() {
	OMPVV_TEST_OFFLOADING;
	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_pointer_with_ternary() != 0);

	OMPVV_REPORT_AND_RETURN(errors);
}
