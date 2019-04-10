#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include "ompvv.h"
#include "ompvv_timing.h"

// Test for OpenMP 4.5 target data with if
int test(int teams) {
  OMPVV_INIT_TIMERS;
  OMPVV_TEST_OFFLOADING;
  printf("TEST_VARIANT\t%d\n",teams);
  int a_map_var = 0;
  OMPVV_INFOMSG("Running timing tests with %d teams", teams);
  //target teams distribute
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute");

  //target teams distribute collapse
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute collapse(1) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute collapse");

  //target teams distribute defaultmap
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute defaultmap(tofrom: scalar) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute defaultmap");

  //target teams distribute default(none)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute default(none) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute default(none)");

  //target teams distribute default(sshared)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute default(shared) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute default(shared)");
  
  //target teams distribute depend
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute depend(inout:a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
          a_map_var = 0;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute depend");
  
  //target teams distribute device
  a_map_var = omp_get_default_device();
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute device(a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute device");

  //target teams distribute firstprivate
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute firstprivate(a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute firstprivate");

  //target teams distribute if(true)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute if(1) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute if(true)");

  //target teams distribute if(false)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute if(0) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute if(false)");

  //target teams distribute lastprivate
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute lastprivate(a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute lastprivate");

  //target teams distribute map(to)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute map(to: a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute map(to)");

  //target teams distribute map(from)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute map(from: a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute map(from)");

  //target teams distribute map(tofrom)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute map(tofrom: a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute map(tofrom)");

  //target teams distribute map(alloc)
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute map(alloc: a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute map(alloc)");

  //target teams distribute private
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute private(a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute private");

  //target teams distribute shared
  OMPVV_INIT_TEST;
  for (int i = 0; i < NUM_REP; ++i){
      OMPVV_START_TIMER;
      #pragma omp target teams distribute shared(a_map_var) num_teams(teams)
      for (int x = 0; x < 1024; ++x){
          a_map_var = 0;
          OMPVV_TIMING_LOAD;
      }
      OMPVV_STOP_TIMER;
      OMPVV_REGISTER_TEST;
  }
  OMPVV_TIMER_RESULT("target teams distribute shared");

  printf("END_OF_TEST\n");
  return 0;
}

int main(){
  for (int x = 0; x < 5; ++x){
    test(1<<x);
  }
}
