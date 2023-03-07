#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include <math.h>

typedef int(*funcptr)();

int fun1(){
	return 5;
}

int fun2(){
	return 10;
}

int fun3(){
	return 15;
}
#pragma omp begin declare enter(fun1, fun2, fun3) indirect

int test_declare_target_indirect(){

        funcptr fptr;
	int errors = 0;
        int fun_choice = rand() % 3; //will give either 0, 1 or 2
	int ret_val, result;
        
	if(!fun_choice){
	  fptr = &fun1;
	  result = 5;
	}
	  
	if(fun_choice == 1){
	  fptr = &fun2;
	  result = 10;
	}
	  
	if(fun_choice == 3){
	  fptr = &fun3;
	  result = 15;
	}	
	  
	#pragma omp target map(from: ret_val)
	  ret_val = fptr();
	  
        OMPVV_TEST_AND_SET(errors, ret_val != result);
	return errors;
}



int main(){

  int errors = 0;
  OMPVV_TEST_OFFLOADING;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_declare_target_indirect() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
	
}
