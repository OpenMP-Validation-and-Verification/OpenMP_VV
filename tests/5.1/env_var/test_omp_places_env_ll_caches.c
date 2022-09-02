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
