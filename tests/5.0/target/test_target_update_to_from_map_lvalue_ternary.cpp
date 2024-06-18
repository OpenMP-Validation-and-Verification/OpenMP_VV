//===------ test_target_update_to_from_map_lvalue_ternary.cpp ----------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test evaluates the target update directive using to and from clauses, 
// supporting various lvalue expressions, such as ternary operators. 
// Additionally, it examines the map clause and its handling of lvalue expressions 
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

	// Map the variable c ? a : b to the target device
	// Should evaluate to a
	#pragma omp target enter data map(to: (c ? a : b))

	// Before should be set to -1 as c is true and a is -1
	#pragma omp target map(from: before_value)
	{
		before_value = c ? a : b;
	}

	// Set c to false
	c = false;

	// Copy data from the host to the device => b
	#pragma omp target update to(c ? a : b)

	// After should be set to 1 as c is false and b is 1
	// b should now be set to 2, as c is false
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
	#pragma omp target update from(c ? a : b)


	// Exit data region, mapping the variable back from the target device to the host
	#pragma omp target exit data map(from: c ? a : b)

	// Verify the results
	// b should be 2 as c is false and we updated it on the device
	if ((c ? a : b) != 2) {
		errors++;
	}
	// before_value should be -1 
	if (before_value != -1){
		errors++;
	}
	// after_value should be 1 since we set b to 1
	if (after_value != 1){
		errors++;
	}

	OMPVV_TEST_AND_SET(errors, errors != 0);
	return errors;
}

int main() {
	OMPVV_TEST_OFFLOADING;
	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_update() != 0);

	OMPVV_REPORT_AND_RETURN(errors);
}

