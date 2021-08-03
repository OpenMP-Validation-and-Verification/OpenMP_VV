//===---- test_declare_target_nested.c ---------------------------------------------===//
// 
// OpenMP API Version 5.0 
//
// The declaration-definition-seq defined by a declare target directive and an end
// declare target directive may contain declare target directives. If a device_type
// clause is present on the contained declare target directive, then its argument 
// determines which versions are made available. If a list item appears both in an
// implicit and explicit list, the explicit list determines which versions are made
// available. 
// 
// @Todo's 1. Test if wrapper is necessary
//===-------------------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024
int arr[N]; // implicit map 3 variables 
int errors = 0;
int i = 0;

void t_add(int *arr);

#pragma omp begin declare variant match( construct={parallel} ) // If inside a parallel for, should use multiply function instead of add
	void t_add(int *arr){
		#pragma omp for
		for (int i = 0; i < N; i ++){
			arr[i] = i + 2;
		}
	}
#pragma omp end declare variant
void add(int *arr){
	for (int i = 0; i < N; i++){
		arr[i] = i+1;
	}
}
//use multiply if in a parallel for, use add outside it
int test_wrapper() { //wrapper for declare target function
    add(arr);
    for(int i =0; i<N; i++){
	OMPVV_TEST_AND_SET_VERBOSE(errors, arr[i] != i+1);
	}
    #pragma omp parallel
    {
	add(arr);
    }
    //print("%d", a[5]);
    //print("%d", b[5]);
    for(int i=0; i<N; i++){
    	OMPVV_TEST_AND_SET_VERBOSE(errors, arr[i] != i+2);
    } 
  return errors;
}

int main () {
  OMPVV_TEST_OFFLOADING;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_wrapper());
  OMPVV_REPORT_AND_RETURN(errors);
}  
