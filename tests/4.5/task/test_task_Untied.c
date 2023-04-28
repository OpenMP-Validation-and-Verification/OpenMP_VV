//===-- test_task_Untied.c ------------------------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// Description
// 100 threads are launched each of which inturn launches
// a task in untied mode. All the threads but the last thread updates the
// global array with values equal to the i value corresponding to the for loop.
// The last thread sleeps for 2 seconds allowing all the threads to finish the
// execution and updates each element with its corresponding position value.
//===----------------------------------------------------------------------===//

#include <stdio.h>
#include <stdbool.h>
#ifdef _WIN32
#include <Windows.h>
#else
#include <unistd.h>
#endif
#include "omp.h"
#include "ompvv.h"

#define THREADS 100


void WaitFunc() {
  usleep(2*1000000);
}

int Arr[THREADS];


int main() {
  int errors = 0, IfTstFailed = 0;
  for (int i = 0; i < THREADS; ++i) {
    Arr[i] = -1;
  }
  omp_set_num_threads(THREADS);  
#pragma omp parallel for
  for (int i = 0; i < THREADS; ++i) {
#pragma omp task untied if (i == (THREADS - 1))
      {
	if (i == (THREADS - 1)) {
          WaitFunc(); // Just to give some time for other threads to
	  // the execution
	  for (int j = 0; j < THREADS; ++j) {
	    if (Arr[j] != j) {
	      Arr[j] = j;
	    }
	  }
	} else {
	  if (Arr[i] != i) {
	    Arr[i] = i;
	  }
      }
  
    }
  }
  // Validating lst if the Thread ids are stored in sequence
  for (int i = 0; i < THREADS; ++i) {
    if (Arr[i] != i) {
      IfTstFailed++;
    }
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, (IfTstFailed != 0));
  OMPVV_REPORT_AND_RETURN(errors);
}
