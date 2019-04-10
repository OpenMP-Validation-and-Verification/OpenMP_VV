#include <assert.h>
#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include "ompvv_timing.h"

int main() {
  OMPVV_INIT_TIMERS;
  OMPVV_TEST_OFFLOADING;
  printf("TEST_TIMING_CLAUSES\n");
  int i = 0;
  int a_map_var = 0;

  // target
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target")

  // target defaultmap
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target defaultmap(tofrom: scalar)
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_defaultmap")

  // target depend
  OMPVV_INIT_TEST
  int dependvar = 0;
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target depend(inout: dependvar)
    {
      dependvar = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_dependvar")

  // target device
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target device(0)
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_device")

  // target firstprivate
  OMPVV_INIT_TEST
  int firstprivatevar = 0;
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target firstprivate(firstprivatevar)
    {
      firstprivatevar = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_firstprivate")

  // target private
  OMPVV_INIT_TEST
  int privatevar = 0;
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target private(privatevar)
    {
      privatevar = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_private")

  // target if
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target if(1)
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_if")

  // target is_device_ptr
  int * device_ptr = omp_target_alloc(sizeof(int), omp_get_default_device());
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target is_device_ptr(device_ptr)
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_is_device_ptr")

  // target map_to
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target map(to: a_map_var)
    {
      a_map_var = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_map_to")

  // target map_from
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target map(from: a_map_var)
    {
      a_map_var = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_map_from")

  // target map_tofrom
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target map(tofrom: a_map_var)
    {
      a_map_var = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_map_tofrom")
  printf("END_OF_TEST\n");
  return 0;
}
