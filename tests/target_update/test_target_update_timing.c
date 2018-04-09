#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include "ompvv_timing.h"

int main() {
  OMPVV_TEST_OFFLOADING;
  OMPVV_INIT_TIMERS;
  int i = 0;
  int a_map_var = 0;

  //// FOR UPDATE TO
#pragma omp target enter data map(to: a_map_var)
  
  // target update to
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update to(a_map_var)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_to")

  // target update to if true
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update to(a_map_var) if (1)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_to_if_true")
  
  // target update to if false
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update to(a_map_var) if (0)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_to_if_false")

  // target update to device
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update to(a_map_var) device(0)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_to_device")
  
  // target update to depend
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update to(a_map_var) depend(inout: a_map_var)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_to_depend")
  
  //// FOR UPDATE FROM
  
  // target update from
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update from(a_map_var)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_from")

  // target update from if true
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update from(a_map_var) if (1)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_from_if_true")
  
  // target update from if false
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update from(a_map_var) if (0)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_from_if_false")

  // target update from device
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update from(a_map_var) device(0)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_from_device")
  
  // target update from depend
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target update from(a_map_var) depend(inout: a_map_var)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_update_from_depend")
 
  
 return 0;
}
