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

  //// FOR ENTER DATA 
  
  // target enter data map to 
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target enter data map(to: a_map_var)
    OMPVV_STOP_TIMER;
#pragma omp target exit data map(delete: a_map_var) // to guarantee remapping 
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_enter_data_map_to")

  // target enter data map alloc
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target enter data map(alloc: a_map_var)
    OMPVV_STOP_TIMER;
#pragma omp target exit data map(delete: a_map_var) // to guarantee remapping 
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_enter_data_map_alloc")

  // target enter data map if true
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target enter data map(alloc: a_map_var) if(1)
    OMPVV_STOP_TIMER;
#pragma omp target exit data map(delete: a_map_var) // to guarantee remapping 
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_enter_data_map_if_true")
  
  // target enter data map if false
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target enter data map(alloc: a_map_var) if(0)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_enter_data_map_if_false")

  // target enter data map device
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target enter data map(alloc: a_map_var) device(0)
    OMPVV_STOP_TIMER;
#pragma omp target exit data map(delete: a_map_var) // to guarantee remapping 
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_enter_data_map_device")
  
  // target enter data map depend
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target enter data map(alloc: a_map_var) depend(inout: a_map_var)
    OMPVV_STOP_TIMER;
#pragma omp target exit data map(delete: a_map_var) // to guarantee remapping 
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_enter_data_map_depend")
 
  //// FOR EXIT DATA 

  // target exit data from
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
#pragma omp target enter data map(alloc: a_map_var)
    OMPVV_START_TIMER;
#pragma omp target exit data map(from: a_map_var) // to guarantee remapping 
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_exit_data_map_from")

  // target exit data map delete
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
#pragma omp target enter data map(alloc: a_map_var)
    OMPVV_START_TIMER;
#pragma omp target exit data map(delete: a_map_var) // to guarantee remapping 
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_exit_data_map_delete")

  // target exit data map if true
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
#pragma omp target enter data map(alloc: a_map_var)
    OMPVV_START_TIMER;
#pragma omp target exit data map(delete: a_map_var) if(1)// to guarantee remapping 
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_exit_data_map_if_true")
  
  // target exit data map if false
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
    OMPVV_START_TIMER;
#pragma omp target exit data map(delete: a_map_var) if(0)
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_exit_data_map_if_false")

  // target exit data map device
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
#pragma omp target enter data map(alloc: a_map_var) 
    OMPVV_START_TIMER;
#pragma omp target exit data map(delete: a_map_var) device(0)// to guarantee remapping 
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_exit_data_map_device")
  
  // target exit data map depend
  OMPVV_INIT_TEST
  for (i = 0; i < NUM_REP; i ++) {
#pragma omp target enter data map(alloc: a_map_var)
    OMPVV_START_TIMER;
#pragma omp target exit data map(delete: a_map_var) depend(inout: a_map_var) // to guarantee remapping 
    OMPVV_STOP_TIMER;
    OMPVV_REGISTER_TEST
  }
  OMPVV_TIMER_RESULT("target_exit_data_map_depend")
  
 return 0;
}
