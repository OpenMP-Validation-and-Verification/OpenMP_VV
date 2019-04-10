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

  // target data map_to
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target data map(to: a_map_var)
    {
      a_map_var = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_data_map_to")

  // target data map_from
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target data map(from: a_map_var)
    {
      a_map_var = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_data_map_from")

  // target data map_tofrom
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target data map(tofrom: a_map_var)
    {
      a_map_var = 1;
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_data_map_tofrom")
  
  // target data device
  OMPVV_INIT_TEST
  int deviceNum = omp_get_num_devices();
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target data map(tofrom: a_map_var) device(0)
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_data_device")

  // target data if
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target data map(tofrom: a_map_var) if(1)
    {
      OMPVV_TIMING_LOAD;
    }
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_data_if")

  // target data is_device_ptr
//  OMPVV_INIT_TEST
//  int * device_ptr = (int *) malloc(sizeof(int));
//  for (i = 0; i < NUM_REP; i ++) {
//    OMPVV_START_TIMER;
//#pragma omp target data map(tofrom: device_ptr[0:1]) use_device_ptr(device_ptr)
//    {
//      OMPVV_TIMING_LOAD;
//    }
//    OMPVV_STOP_TIMER;
//    OMPVV_REGISTER_TEST
//  }
//  OMPVV_TIMER_RESULT("target_data_use_device_ptr")

  printf("END_OF_TEST\n");
  return 0;
}
