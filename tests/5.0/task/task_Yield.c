/*Test Description: launch 100 threads. Each thread launches 2 wait Functions
 * with conditional yield between them. Measure the time difference between
 * the two runs one with taskyield enabled and one without.
 * Expectation: The run with taskyield enabled should take more time.
 *
 * */

#include <stdio.h>
#include <time.h>
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
  usleep(500); // Waiting in multiples of 50 microseconds based on thread id
}

int arr[THREADS];
bool YIELD_TASK = true;

double LaunchThreads () {
  omp_set_num_threads(THREADS);
  clock_t t;
  double TimeElapsed = 0;
  t = clock();
#pragma omp parallel for
  for (int i = 0; i < THREADS; ++i) {
    #pragma omp task untied
      {
        WaitFunc();
        if (YIELD_TASK) {
          #pragma omp taskyield
        }
        WaitFunc();
      }
  }
  t = clock() - t;
  TimeElapsed = ((double)t)/CLOCKS_PER_SEC;
  return TimeElapsed;

}

int main() {
  int errors = 0, IfTstFailed = 0;
  omp_set_num_threads(THREADS);  
  double Run1 = 0, Run2 = 0;
  Run1 = LaunchThreads();

  YIELD_TASK = false;
  Run2 = LaunchThreads();

  // Note: The below condition is true almost always only when threads = 100
  if (Run2 > Run1) {
    IfTstFailed++;
  }
  OMPVV_TEST_AND_SET_VERBOSE(errors, (IfTstFailed != 0));
  OMPVV_REPORT_AND_RETURN(errors);
}
