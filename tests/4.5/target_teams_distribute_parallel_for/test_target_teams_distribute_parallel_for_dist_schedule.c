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
#define NUM_TEAMS 2

int Dist_ScheduleStatic() {
  int ErrCount = 0;

  int TrackUsedThread[NUM_TEAMS][THRD];
  for (int j = 0; j < NUM_TEAMS; ++j) {
    for (int i = 0; i < THRD; ++i) {
      TrackUsedThread[j][i] = 0;
    }
  }

#pragma omp target teams distribute parallel for num_teams(NUM_TEAMS)\
        thread_limit(THRD)  map(tofrom: TrackUsedThread[0:NUM_TEAMS], num_teams, num_threads)\
        dist_schedule(static, 4)
  for (int i = 0; i < 32; ++i) {
    TrackUsedThread[omp_get_team_num()][omp_get_thread_num()] += 1;
  }

  // Check - 1: First four columns of each row should not be zero
  for (int i = 0; i < NUM_TEAMS; ++i) {
    for (int j = 0; j < 4; ++j) {
      if (TrackUsedThread[i][j] == 0) {
        ErrCount++;
      }
    }
  }
  // Check - 2: column index 4 to 9 must be zero as we used dist_schedule(static, 4)
  for (int i = 0; i < NUM_TEAMS; ++i) {
    for (int j = 4; j < THRD; ++j) {
      if (TrackUsedThread[i][j] != 0) {
        ErrCount++;
      }
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
