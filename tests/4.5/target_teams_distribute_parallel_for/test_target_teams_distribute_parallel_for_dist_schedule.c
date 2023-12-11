//===-test_target_teams_distribute_parallel_for_dist_schedule.c-===//
//
// OpenMP API Version 4.5 Nov 2015
//
// testing the combined construct target teams distribute parallel for
// This test will check if dist_schedule(static, 4) taking effect
//
//===---------------------------------------------------------------------===//

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"

#define THRD 10

int Dist_ScheduleStatic() {
  int ErrCount = 0;
  int Arr[32];
  for (int i = 0; i < 32; ++i) {
    Arr[i] = i;
  }

  int TrackUsedThread[THRD];
  for (int i = 0; i < THRD; ++i) {
    TrackUsedThread[i] = -1;
  }

#pragma omp target teams distribute parallel for num_teams(2)\
        thread_limit(THRD)  map(tofrom: TrackUsedThread[0:THRD])\
        dist_schedule(static, 4)
  for (int i = 0; i < 32; ++i) {
    if (Arr[i] != i) {
#pragma omp atomic
      ErrCount += 1;
    }
    TrackUsedThread[omp_get_thread_num()]++;
  }


  for (int i = 0; i < 4; ++i) {
    if (TrackUsedThread[i] == -1) {
      ErrCount++;
    }
  }

  for (int i = 4; i < THRD; ++i) {
    if (TrackUsedThread[i] != -1) {
      ErrCount++;
    }
  }
  return ErrCount;
}



int main() {
  OMPVV_TEST_OFFLOADING;
  int errors = 0;
  OMPVV_TEST_AND_SET_VERBOSE(errors, Dist_ScheduleStatic() != 0);
  OMPVV_REPORT_AND_RETURN(errors);
}
