//------------------------------------------- test_taskloop_num_tasks.c --------------------------------------//
//
// OpenMP API Version 4.5 September 2015
//
// This test checks the 'taskloop' directive with the 'num_tasks' clause specified.
// The 'taskloop' construct parallelizes loops with independent iterations by creating tasks. 
// It allows for efficient parallel execution of loop iterations by distributing them among multiple threads. 
// The 'num_tasks' clause variable ensures that the loop iterations are shared amoung created 'num_tasks'. 
// i.e the loop iterations are divided amoung number of tasks (each num_task group is run by same the thread).   
//----------------------------------------------------------------------------------------------------------//

#include <stdio.h>
#include <omp.h>
#include "ompvv.h"

#define NUM_THREADS 100
#define NUM_TASKS 4

int test_taskloop_num_tasks() {

  int errors = 0;

  long int all_thread_sum, real_sum = 0; 
  long int var = 0; //This variable is shared with all the tasks.  
  
  int NUM = 100;

   //get valid sum without openmp
   for(int i = 0; i < NUM; i++) {
     real_sum = real_sum + i;
   }

   int thread_ids[100];
   int values[100];

   #pragma omp parallel num_threads(NUM_THREADS)
   {
      #pragma omp single
      {
	#pragma omp taskloop num_tasks(NUM_TASKS) 
        for(int i = 0; i < NUM; i++) 
	{
	    #pragma omp atomic 
	    var = var + i;

	    values[i] = var;
	    thread_ids[i] = omp_get_thread_num();
	}
      }	   
   }

   int threads_max = values[0]; // Assume the first element is the maximum
   
   for(int i = 0; i < NUM; i++) {

	   if(values[i] > threads_max) 
	   	threads_max = values[i]; 
   }

   all_thread_sum = threads_max;

   OMPVV_TEST_AND_SET_VERBOSE(errors, real_sum != all_thread_sum);

   return errors;
}

int main() {
  
  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_num_tasks()) != 0);
  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}
