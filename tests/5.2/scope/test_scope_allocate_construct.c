//--------------- test_scope_allocate_construct.c -----------------------------//
//
// OpenMP API Version 5.2 Nov 2021
//
// This test checks that the scope directive with the allocate clause is
// working correctly.
// This is done by specifying
//----------------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_allocate(){
        int errors=0;
        int arr[N];

	#pragma omp target map(tofrom: errors) 
        #pragma omp scope private(arr) allocate(allocator(omp_low_lat_mem_alloc): arr)
        {
		for(int i=0; i< N; i++){
                	arr[i] = i;
                }
		for(int j=0; j< N; j++){
			if(arr[j] != j) errors++;
		}
        }
        return errors;
}

int main(){
        int errors = 0;
        OMPVV_TEST_OFFLOADING;
        OMPVV_TEST_AND_SET_VERBOSE(errors, test_allocate() != 0);
        OMPVV_REPORT_AND_RETURN(errors);
}
