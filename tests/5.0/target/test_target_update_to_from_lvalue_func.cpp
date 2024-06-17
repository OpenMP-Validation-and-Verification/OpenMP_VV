//===----------- test_target_update_to_from_map_lvalue_fuc.cpp ----------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This test evaluates the target update directive using to and from clauses, 
// supporting various lvalue expressions, such as functions that return references.
//  Additionally, it examines the map clause and its handling of lvalue expressions. 
//===--------------------------------------------------------------------------===//
#include <omp.h>
#include <iostream>
#include "ompvv.h"

//Global Variable
int var;


//Map the variable to the device using the function
#pragma omp declare target to(var)


//Function that will return a reference to the variable var
int& returnRef() {
	return var;
}



int test_target_update(){
	int errors = 0;
	int before_value;
	int after_value;

	//Initialize var
	var = -1;

	//Set before_value to the initial value of var from the device
	#pragma omp target map(from: before_value)
	{
		before_value = returnRef();
	}

	//Update the value of var on the host
	var = 1;

	//Update the device with the new value of var from the host
	#pragma omp target update to(returnRef())

	//Set after_value with the update value of var and update var on the device
	#pragma omp target map(from: after_value)
	{
		after_value = returnRef();
		returnRef() = 2;
	}

	//Update the host with the new value of var form the device
	#pragma omp target update from(returnRef())

	//Check for Errors
	if (var != 2){
		errors++;
	}
	if (before_value != -1){
		errors++;
	}
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
