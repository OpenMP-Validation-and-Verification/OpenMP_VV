//-------------test_scope_nowait_construct.c-------------//
//
//OpenMP API Version 5.1 Aug 2021
//
//Tests the behavior of the scope construct with nowait
//specified. This test should remove the implied barrier
//creatd by the scope construct when nowait is specified.
//-------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>
#include <unistd.h>

#define N 1024

int a = 1;
int b = 2;
int c = 3;
int d = 4;
int sum1 = 0;
int sum2 = 0;

void test_func1(){
	sum1 = a+b;
	return;
}

int test_func2(){
	sum2 = c+d;
	return;
}

int test_scope_nowait(){
	int errors = 0;
	#pragma omp scope
	{
		test_func1();
	}
	#pragma omp scope nowait
	#pragma omp scope
	{
		test_func2();
	}
	int total = sum1 + sum2
	OMPVV_TEST_AND_SET_VERBOSE(errors, total != 10);
        OMPVV_INFOMSG_IF(sum1 == 0, "first function was not called");
	OMPVV_INFOMSG_IF(sum2 == 0, "second function was not called");
	return errors;
}

int main(){
	int errors = 0;
	OMPVV_TEST_OFFLOADING;
	OMPVV_TEST_AND_SET_VERBOSE(errors, test_scope_nowait() != 0);
	OMPVV_REPORT_AND_RETURN(errors);
}
