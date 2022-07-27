#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int test_case(){
	int errors = 0;
	int env = 10;
	setenv("OMP_PLACES", "{0,1,2,3},{4,5,6,7}",1);
	env = omp_get_num_places();
	OMPVV_TEST_AND_SET_VERBOSE(errors, env != 2);
	OMPVV_INFOMSG_IF(env == 0, "Environment variable was not set");
	OMPVV_INFOMSG_IF(env == 10, "Call to omp_get_num_places failed");
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_case() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
