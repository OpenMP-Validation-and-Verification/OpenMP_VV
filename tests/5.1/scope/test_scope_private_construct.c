#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int test_scope(){
	int errors;
	int test_int = 1;
	#pragma omp parallel firstprivate(test_int)
	{
		#pragma omp task private(test_int)
		{
			test_int += 1;
		}
	}
	OMPVV_TEST_AND_SET_VERBOSE(errors,test_int != 1);
	OMPVV_INFOMSG_IF(test_int == 2, "test int was not private");
	return errors;
}

int main(){
	int errors;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_scope() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
	return errors;
}
