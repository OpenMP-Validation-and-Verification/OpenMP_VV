//===------ test_target_map_lvalue_ternary.cpp ----------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test evaluates the map clause and its ability to handle various lvalue 
// expressions, such as ternary operators.
//
//===-------------------------------------------------------------------------===//

#include <omp.h>
#include <iostream>
#include "ompvv.h"

int test_target_update() {
	int errors = 0;
	int before_value;
	int after_value;
	int a = -1;
	int b = 1;
	bool c = true;

	// Map the variables a and b. 
	#pragma omp target enter data map(to: c ? a : b)

	// Before should be set to -1 as c is true and a is -1
	#pragma omp target map(from: before_value)
	{
		before_value = c ? a : b;
	}

	// Copy data from the host to the device => b
	#pragma omp target update to(a)

	// After should be set to -1 as c is true and a is now  2
	#pragma omp target map(from: after_value)
	{
		after_value = c ? a : b;
		if (c){
			a = 2;
		}
		else {
			b = 2;
		}
	}

	// Copy the updated value from the device to the host variable => b = 2
	#pragma omp target update from(a)

	// Exit data region, mapping the variable back from the target device to the host
	#pragma omp target exit data map(delete: c ? a : b)

	// Verify the results
	OMPVV_TEST_AND_SET(errors, (c ? a : b) != 2); 	
	OMPVV_TEST_AND_SET(errors, before_value != -1);
	OMPVV_TEST_AND_SET(errors, after_value != -1);

	return errors;
}

int main() {

	OMPVV_TEST_OFFLOADING;

	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_update() != 0);

	OMPVV_REPORT_AND_RETURN(errors);
}

