//---------------------------------------- test_taskloop_private.c -----------------------------------------//
//
// OpenMP API Version 4.5 August 2015
//
// This test checks the taskloop directive with the 'private' clause specified.
// The 'taskloop' construct parallelizes loops with independent iterations by creating tasks. 
// It allows for efficient parallel execution of loop iterations by distributing them among multiple threads. 
// The 'private' clause ensures that each task should have private copies. 
//----------------------------------------------------------------------------------------------------------//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define NUM_THREADS 100
#define NUM_TASKS 100

int test_taskloop_private() {

  int errors = 0;

  int private_var = -2; 
  int shared_var_sum = 0;	
  int num_threads = 0;
  
   #pragma omp parallel num_threads(NUM_THREADS)
   {
      num_threads = omp_get_num_threads();
      #pragma omp single
      {
	#pragma omp taskloop private(private_var) 
        for(int i = 0; i < NUM_TASKS; i++) {
	  private_var = omp_get_thread_num();
          #pragma omp atomic
	    shared_var_sum += private_var;
	 }      
      }	   
   }
  
   //check if the private variable is same before and after openmp region. i.e it should be same
   OMPVV_TEST_AND_SET_VERBOSE(errors, private_var != -2);
   if(num_threads == 100){
     OMPVV_TEST_AND_SET_VERBOSE(errors, shared_var_sum != NUM_THREADS * ((NUM_THREADS + 1)/2) );
   
   return errors;
}


int main() {
  
  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_private()) != 0);
  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}

