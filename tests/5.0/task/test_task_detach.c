//===--- test_task_detach.c -------------------------------------------------===//
//
// OpenMP API Version 5.0 Nov 2018
//
// This is a test of the 5.0 task detach clause. A task with the detach
// should not complete until its associated block and the event in the
// detach clause is fulfilled with the omp_fulfill_event clause. This test
// confirms that the task will not complete until these conditions are
// met by placing a dependent task after the detached task. Variables are
// set inside the task body and in the function that fulfills the event,
// and their values are checked after the depend to ensure they are set as
// expected. This test is based on the example of task detach presented
// by Michael Klemm at the 2018 OpenMPCon.
//
////===----------------------------------------------------------------------===//

#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define N 1024

int test_callback(omp_event_handle_t event) {
  omp_fulfill_event(event);
  return 1;
}

int test_task_detach() {
  OMPVV_INFOMSG("test_task_detach");
  int errors = 0, x = 0, y = 0;
  int num_threads = -1, record_x = -1, record_y = -1;
  omp_event_handle_t flag_event;

#pragma omp parallel
#pragma omp single
  {
#pragma omp task depend(out: y) detach(flag_event)
    {
      y++;
    }
#pragma omp task
    {
      x = 1;
#pragma omp flush
      test_callback(flag_event);
    }
#pragma omp task depend(inout: y)
    {
#pragma omp flush
      record_x = x;
      record_y = y;
      num_threads = omp_get_num_threads();
    }
  }

  OMPVV_ERROR_IF(num_threads < 0, "Test ran with invalid number of teams (less than zero)");
  OMPVV_WARNING_IF(num_threads == 1, "Test ran with one thread, so the results are not conclusive");

  OMPVV_TEST_AND_SET_VERBOSE(errors, record_x != 1);
  OMPVV_ERROR_IF(record_x == 0, "Depend did not wait for associated event to be fulfilled");
  OMPVV_ERROR_IF(record_y == 0, "Depend did not wait for task body to execute");
  OMPVV_ERROR_IF(record_x == -1 || record_y == -1, "Recording variables were not set correctly")

    return errors;
}

int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_detach());

  OMPVV_REPORT_AND_RETURN(errors);
}
