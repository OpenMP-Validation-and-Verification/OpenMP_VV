#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>




int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_nowait() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
