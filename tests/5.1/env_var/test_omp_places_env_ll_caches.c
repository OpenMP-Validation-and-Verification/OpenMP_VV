//-----------------test_scope_nowait_construct.c--------------//
//
//OpenMP API Version 5.1 Aug 2021
//
//Tests the omp_places environment variable. This test sets the
//omp_places environment variable and then retrives it form the
//environment. If the architecture supports the ll_caches 
//argument then the retrived value will be ll_caches.
//-------------------------------------------------------------

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int test_places(){
	int errors = 0;
	int test = 1;
	char* ret_val = ""; 
	setenv("OMP_PLACES", "ll_caches", 1 );
	ret_val = getenv("OMP_PLACES");
	test = strcmp(ret_val, "ll_caches");
	OMPVV_TEST_AND_SET(errors,test != 0);
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_places() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
