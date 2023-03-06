#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

typedef int(*funcptr)();

int function1(){
	return 0;
}

int function2(){
	return 0;
}

#pragma omp begin declare enter(function1, function2) indirect
//enter 5.2 feature?

int main(){
	funcptr fptr = (choice == 1) ? &fun1:&fun2;
	#pragma omp target map(from: ret_val)
		ret_val = fptr();
}
