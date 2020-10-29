//===--- test_task_detach.c -------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This is a test of the 5.0 task detach clause. The clause should cause the
// task construct to detach and not complete, causing the following taskwait
// to block thread0 until thread1 fulfills the event the task is detached to.
// Order of events should be: code before omp_fulfill_event() ~~ code in task
// --> code after taskwait. This order is checked by recording the value of
// variables set before the call to omp_fulfill_event() and in the task.
//
////===----------------------------------------------------------------------===//
#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_task_detach() {
  OMPVV_INFOMSG("test_task_detach");
  int errors = 0, x = 0, y = 0;
  int num_threads = -1, record_x = -1, record_y = -1;
  omp_event_handle_t *flag_event;

#pragma omp parallel num_threads(2) default(shared)
  {
    if (omp_get_thread_num() == 1 || omp_get_num_threads() < 2) {
      num_threads = omp_get_num_threads();
      x++;
      omp_fulfill_event(flag_event);
    }
    if (omp_get_thread_num() == 0) {
#pragma omp task detach(flag_event)
      {
        y++;
      }
#pragma omp taskwait
      record_x = x;
      record_y = y;
    }
  }

  OMPVV_ERROR_IF(num_threads < 0, "Test ran with invalid number of teams (less than zero)");
  OMPVV_WARNING_IF(num_threads == 1, "Test ran with one thread, so the results are not conclusive");

  OMPVV_TEST_AND_SET_VERBOSE(errors, record_x != 1 || record_y != 1);
  OMPVV_ERROR_IF(record_x == 0, "Taskwait did not wait for associated event to be fulfilled");
  OMPVV_ERROR_IF(record_y == 0, "Taskwait did not wait for task body to execute");
  OMPVV_ERROR_IF(record_x == -1 || record_y == -1, "Recording variables were not set in the parallel region as expected.")

  return errors;
}


int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_detach());

  OMPVV_REPORT_AND_RETURN(errors);
}
