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

#define NUM_TASKS 6
#define NUM_ITERATIONS 12

int isGroupIdsSame(int thread_ids[])
{
        int iterationsPerGroup = NUM_ITERATIONS / NUM_TASKS;

        for(int i = 0; i < NUM_ITERATIONS; i = i+iterationsPerGroup)
        {
                for(int j = 0; j<iterationsPerGroup; j++) {
                        if (thread_ids[i+j] != thread_ids[i]) {
                                return 0; // Return false if any id is different in a group
                        }
                }
        }

        return 1; // Return true if all id's are same per group
}

int test_taskloop_num_tasks() {

  int errors = 0;

  long int var = 0; //This variable is shared with all the tasks.

   int thread_ids[NUM_THREADS];
   int values[NUM_THREADS];

   #pragma omp parallel num_threads(OMPVV_NUM_THREADS_HOST) // 8 threads requested
   {
   if (omp_get_thread_num() == 0){
     if (omp_get_num_threads()==1)
       OMPVV_WARNING("The parallel region is executing single threaded.");
       }
      }

      #pragma omp single
      {
        #pragma omp taskloop num_tasks(NUM_TASKS)
        for(int i = 0; i < NUM_ITERATIONS; i++)
        {
            #pragma omp atomic
            var = var + i;

            values[i] = var;
            thread_ids[i] = omp_get_thread_num();
        }
      }
   }

   //if all the tasks in a group are run by a same thread, get TRUE else FALSE
   OMPVV_WARNING_IF("The tasks were ran by a single thread", (isGroupIdsSame(thread_ids) != 1));
   OMPVV_TEST_AND_SET_VERBOSE(errors, var != ((NUM_ITERATIONS-1)*(NUM_ITERATIONS)/2);

   return errors;
}

int main() {
  
  int errors = 0;
  
  OMPVV_TEST_AND_SET_VERBOSE(errors, (test_taskloop_num_tasks()) != 0);
  OMPVV_REPORT_AND_RETURN(errors);

  return 0;
}
