//--------------------------------------------- test_taskloop_final.c ----------------------------------------//
//
// OpenMP API Version 4.5 August 2015
//
// This test checks the taskloop directive with the if clause specified.
// The 'taskloop' construct parallelize loops with independent iterations by creating tasks. 
// It allows for efficient parallel execution of loop iterations by distributing them among multiple threads. 
// The 'final clause is used to ensure that all tasks in taskloop are run by same thread if the 
// final(expression) is TRUE, else different threads are run by each task.      
//----------------------------------------------------------------------------------------------------------//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define NUM_TASKS 10
#define NUM_THREADS 10

#define M 100

int thread_ids[NUM_TASKS][3]; //Array 2nd diemention value i.e 3 is depend on number of omp task under taskloop


//if all thread ids are same, val=1 else val=0
int thread_ids_are_same_in_task() {

 	int val = -1; 
 	for(int i = 0; i < NUM_TASKS; i++) 
	{
	 	int first = thread_ids[i][0];

	 	for(int j = 1; j < 3; j++) 
	 	{
	        	if(first != thread_ids[i][j]) {
		  		val = 0;  //if all the thread ids are not same per task
		  		break; 
			}
			else if(first == thread_ids[i][j]) {
		    		val = 1; //if all the thread ids are same per task
			}
   		}
	 
		if(val == 0)
	       		break;	
   	}

	return val;
}

int test_taskloop_final(int THRESHOLD) {
  
  int errors = 0; 
  
  #pragma omp parallel num_threads(NUM_THREADS)
  {
  	#pragma omp single
    	{	
        	#pragma omp taskloop final(M == THRESHOLD)
      		for (int i = 0; i < NUM_TASKS; i++) {
			#pragma omp task 
			{
	 			thread_ids[i][0] = omp_get_thread_num();
			}
			#pragma omp task 
			{
	 			thread_ids[i][1] = omp_get_thread_num();
			}

			#pragma omp task 
			{
	 			thread_ids[i][2] = omp_get_thread_num();
			}
      		} 
   	}
  }

 int val = thread_ids_are_same_in_task();
 
  if (M == THRESHOLD) {
    if (val != 1) {
      errors++; 
    }
  } else if (M != THRESHOLD) {
    if (val != 0) {
      errors++; 
    }
  }

  return errors;            

}

int main() {
 
   int errors = 0;
   
   int THRESHOLD = 100; //Equal to M
   OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_final(THRESHOLD) != 0));

   THRESHOLD = 110; // Not equal to M
   OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_final(THRESHOLD) != 0));

   OMPVV_REPORT_AND_RETURN(errors);

   return 0;
}
