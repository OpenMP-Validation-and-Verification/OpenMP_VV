//===-- test_task_depend_iterator.cpp - test of the iterator clause inside the task depend clause ----===//
// 
// OpenMP API Version 5.0 Nov 2020
// 
// This file is a test for the iterator modifier when used with the task depend
// clause. This modifier should create an iterator that expand into multiple values
// inside the clause they appear. In this particular test case the iterator expands into
// several values creating several dependencies at the same time.
//
//===-------------------------------------------------------------------------------------------------===//

#include <omp.h>
#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <iostream>
#include <thread>
#include <vector>
#include "ompvv.h"

int test_task_depend_iterator() {
  int ptr[] = {0, 4, 5, 6, 7, 8, 9, 10, 11}, cols[] = {1, 2, 3, 4, 5, 5, 6, 6, 7, 7, 8};
  std::vector<int> threadOrder, threadAssignment;
  bool threadOrderError=false, threadAssignmentWarning=false;
#pragma omp parallel num_threads(4)
  {
#pragma omp single
    {
      for (int i = 0; i < 8; ++i) {
        int pos = ptr[i], size = ptr[i+1] - ptr[i];
#pragma omp task depend(iterator(it = 0 : size), in : ptr[cols[pos+it]]) depend(out : ptr[i])
        {
#pragma omp critical
          {
            threadOrder.push_back(i);
            threadAssignment.push_back(omp_get_thread_num());
          } // end critical section
          std::this_thread::sleep_for(std::chrono::milliseconds(100));
        } // end task depend
      }
    } // end single
  } // end parallel
  std::sort(threadOrder.begin()+1,threadOrder.begin()+5);
  std::sort(threadOrder.begin()+5,threadOrder.begin()+7);
  std::sort(threadAssignment.begin()+1,threadAssignment.begin()+5);
  for(int i = 0; i < 8; ++i)
    threadOrderError = (threadOrder[i] != i) || threadOrderError;
  for(int i = 0; i < 4; ++i)
    threadAssignmentWarning = (threadAssignment[i+1] != i) || threadAssignmentWarning;
  threadAssignmentWarning = (threadAssignment[5] == threadAssignment[6]) || threadAssignmentWarning;
  OMPVV_WARNING_IF(threadAssignmentWarning, "The thread id assigned to each task may be erroneous.");
  OMPVV_ERROR_IF(threadOrderError, "The dependencies between tasks were not enforced in the correct order.");
  return threadOrderError ? 1 : 0;
}


int main() {
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, test_task_depend_iterator());
  OMPVV_RETURN(errors);
}
