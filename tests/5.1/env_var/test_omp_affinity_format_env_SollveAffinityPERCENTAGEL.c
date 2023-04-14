//--------------- test_omp_affinity_format_env_SollveAffinityPERCENTAGEL.c ---------------------//
//
// OpenMP API Version 5.1 Aug 2021
//
// The objective of this test is to check that the OMP_AFFINITY_FORMAT env
// var is read properly it is expected to be SollveAffinity%L. The Makefile 
// or then ran setenv OMP_AFFINITY_FORMAT SollveAffinity%L or
// export OMP_AFFINITY_FORMAT=SollveAffinity%L
// The makefile detects PERCENTAGE and changes it to % when ran
// It is important to note that the name of the test
// is very important to the env var being set. It follows a convention set
// up for testing env variables in run.sh. This test itself simply checks
// that the affinity format is set to the appropriate variable value specified
// in the test name. 
//-----------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

int test_case() 
{
	int errors = 0;
	char expected[] = "SollveAffinity%L"; //test name after _env_ and before .c
	int expectedSize = sizeof(expected);
	char buffer[expectedSize];
	
	omp_display_affinity(NULL); // displays affinity expected SollveAffinity0
	int readSize = omp_get_affinity_format(buffer, expectedSize);
	printf("Affinity read = %s\n",buffer);

	for (int i = 0;i < readSize;i++)
		OMPVV_TEST_AND_SET_VERBOSE(errors, expected[i] != buffer[i]);

	OMPVV_TEST_AND_SET_VERBOSE(errors, readSize == expectedSize);
	
	OMPVV_INFOMSG_IF(readSize == 0, "Environment variable not set");
	OMPVV_INFOMSG_IF(readSize != expectedSize, "OMP_AFFINITY_FORMAT read different than expected");
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_case() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
