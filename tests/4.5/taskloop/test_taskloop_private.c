//---------------------------------------- test_taskloop_private.c -----------------------------------------//
//
// OpenMP API Version 4.5 August 2015
//
// This test checks the taskloop directive with the 'private' clause specified.
// The 'taskloop' construct parallelize loops with independent iterations by creating tasks. 
// It allows for efficient parallel execution of loop iterations by distributing them among multiple threads. 
// The 'private' clause ensures that each task should have private copies. 
//----------------------------------------------------------------------------------------------------------//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define NUM_THREADS 100
#define NUM_TASKS 1000

int test_taskloop_private() {

  int errors = 0;

  int private_var = 10;   

  int private_var_sum = private_var; 
  int val_not_in_openmp = private_var;
  
   #pragma omp parallel num_threads(NUM_THREADS)
   {
      #pragma omp single
      {
	#pragma omp taskloop private(private_var_sum) 
        for(int i = 0; i < NUM_TASKS; i++) {
	  private_var_sum = private_var_sum + i;
	 }      
      }	   
   }
  
   //check if the private variable is same before and after openmp region. i.e it should be same
   OMPVV_TEST_AND_SET_VERBOSE(errors, val_not_in_openmp != private_var_sum);
   
   return errors;
}


int main() {
  
  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_private()) != 0);
  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}

