//===------ test_target_update_to_from_map_lvalue_pointer.cpp ----------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// Tests the target update directive with to and from clauses supporting more
// lvalue expressions
//===----------------------------------------------------------------------===//

#include <omp.h>
#include <iostream>
#include "ompvv.h"

int test_target_update() {
	int errors = 0;
	int before_value;
	int after_value;
	int* host_pointer = new int;
	*host_pointer = -1;

	#pragma omp target enter data map(to: *host_pointer)

	//Before should be set to -1
	#pragma omp target map(from: before_value)
	{
		before_value = *host_pointer;
	}

	//Host_pointer should now be 1
	*host_pointer = 1;
	// Copy data from the host variable to the device variable using a dereferenced pointer
	#pragma omp target update to(*host_pointer)

	#pragma omp target map(from: after_value)
	{
		after_value = *host_pointer;
		*host_pointer = 2;
	}

	//Update host_pointer to 2
	#pragma omp target update from(*host_pointer)

	// Verify the results
	if (*host_pointer != 2) {
		errors++;
	}
	if (before_value != -1){
		errors++;
	}
	if (after_value != 1){
		errors++;
	}

	#pragma omp target exit data map(from: *host_pointer)
	delete host_pointer;

	OMPVV_TEST_AND_SET(errors, errors != 0);
	return errors;
}

int main() {
	OMPVV_TEST_OFFLOADING;
	int errors = 0;

	OMPVV_TEST_AND_SET_VERBOSE(errors, test_target_update() != 0);

	OMPVV_REPORT_AND_RETURN(errors);
}

