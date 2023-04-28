//===-- test_task_Mergeable.c ------------------------------------------------===//
//
// OpenMP API Version 4.5 Nov 2015
//
// Description
// Test case description: This test case demonstrates that mergeable clause
// help thread avoid task creation can queing. The if condition turns out
// false for thread 10, this enables the thread 10 acquire more tasks from
// the queue and run resulting in higher count value.
// 
//===----------------------------------------------------------------------===//


#include <stdio.h>
#include <stdbool.h>
#include <unistd.h>
#include "omp.h"
#include "ompvv.h"
#define THREADS 64


bool IfTstPassed = true;
int arrA[THREADS];
int GlobalThrdId = -1;
int count = 0;

/*void WaitFunc() {
  usleep(1000);
}*/

void WaitFunc2() {
  usleep(1*1000*1000);
}

void foo() {
  int ThrdId = omp_get_thread_num();

#pragma omp task mergeable if(ThrdId != 10)
  {
    GlobalThrdId = ThrdId;
    arrA[ThrdId] = ThrdId;
  }
#pragma omp critical
  {
    if (GlobalThrdId == 10) {
      count++;
    }
  }
  WaitFunc2();
}


int main() {
  int errors = 0, TstFailed = 0;
  omp_set_num_threads(THREADS);
  for (int i = 0; i < THREADS; ++i) {
    arrA[i] = -1;
  }
#pragma omp parallel
  foo();

  if (count < (THREADS * 0.75)) {
    TstFailed++;
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, (TstFailed != 0));
  OMPVV_REPORT_AND_RETURN(errors);
}
