#ifndef __OMPVV_TIMING__
#define __OMPVV_TIMING__

#include "ompvv.h"
#include <math.h>
#include <sys/time.h>
#include <stdint.h>


#ifndef CUDA_CUPTI
#define NUM_REP 3
#define OMPVV_GET_TIME(timer) \
{ \
  struct timeval time;\
  gettimeofday(&time, NULL);\
  timer = (uint64_t)time.tv_sec*1e6 + (uint64_t)time.tv_usec; \
}

#define OMPVV_PRINT_TIME_LAPSED(start, stop) \
{ \
  OMPVV_INFOMSG("Time(us) = %ju", stop - start)\
}


#define OMPVV_INIT_TIMERS \
  uint64_t _ompvv_aux, _ompvv_tmp, _ompvv_start, _ompvv_stop, _ompvv_partial, _ompvv_cur_result = 0, _ompvv_all_results[NUM_REP]; \
  uint64_t _ompvv_time_stamps[NUM_REP]; \
  uint64_t _ompvv_accum; \
  double _ompvv_average, _ompvv_std_dev, _ompvv_median;

#define OMPVV_TIMING_LOAD \
{ \
  uint64_t volatile __ompvv_b = 0;\
    __ompvv_b++; \
}

#define OMPVV_START_TIMER OMPVV_GET_TIME(_ompvv_start)
#define OMPVV_STOP_TIMER OMPVV_GET_TIME(_ompvv_stop)
#define OMPVV_GET_TIME_LAPSED (_ompvv_stop - _ompvv_start)

#define OMPVV_INIT_TEST \
  for (_ompvv_aux = 0; _ompvv_aux < NUM_REP; _ompvv_aux++) { _ompvv_all_results[_ompvv_aux] = -1; } \
  _ompvv_cur_result = 0; \
  _ompvv_accum = 0; 

// Implementing insertion sort 
#define OMPVV_REGISTER_TEST \
   _ompvv_partial = OMPVV_GET_TIME_LAPSED; \
  for (_ompvv_aux = 0; _ompvv_aux < _ompvv_cur_result+1; _ompvv_aux++) { \
    if (_ompvv_partial < _ompvv_all_results[_ompvv_aux]) { \
      _ompvv_tmp = _ompvv_all_results[_ompvv_aux]; \
      _ompvv_all_results[_ompvv_aux] = _ompvv_partial; \
      _ompvv_partial = _ompvv_tmp; \
    } \
  } \
  _ompvv_cur_result++;
  


// Find average, median and standard deviation ignore first and last (list of results is sorted)
#define OMPVV_TIMER_RESULT(clause) \
  for (_ompvv_aux = 1; _ompvv_aux < NUM_REP-1; _ompvv_aux++) \
    _ompvv_accum += _ompvv_all_results[_ompvv_aux]; \
  _ompvv_average = (double)_ompvv_accum / (NUM_REP-2); \
  _ompvv_accum = 0; \
  for (_ompvv_aux = 1; _ompvv_aux < NUM_REP-1; _ompvv_aux++) \
    _ompvv_accum += pow(_ompvv_all_results[_ompvv_aux] - _ompvv_average, 2); \
  _ompvv_std_dev = sqrt(_ompvv_accum/(NUM_REP-2)); \
  _ompvv_median = ((NUM_REP-2) % 2 == 0) ? (_ompvv_all_results[(NUM_REP-2)/2] + _ompvv_all_results[(NUM_REP-2)/2 + 1])/2 : _ompvv_all_results[(NUM_REP-2)/2 + 1];\
  OMPVV_INFOMSG("["clause"] AVG_TIME = %f us, STD_DEV = %f us, MEDIAN = %f us, MAX_TIME = %ju us, MIN_TIME = %ju us", _ompvv_average, _ompvv_std_dev, _ompvv_median, _ompvv_all_results[NUM_REP-1], _ompvv_all_results[1]);

#define OMPVV_PRINT_VALUES \
  for (_ompvv_aux = 0; _ompvv_aux < NUM_REP; _ompvv_aux++) { OMPVV_INFOMSG("%ju", _ompvv_all_results[_ompvv_aux]); }



#else // CUDA PROFILER VERSION

#define NUM_REP 1

#include <stdio.h>
#include <cuda.h>
#include <cupti.h>
#include "ompvv_cupti.h"
#define OMPVV_INIT_TIMERS  \
initTrace();

#define OMPVV_PRINT_TIME_LAPSED(start, stop) 

#define OMPVV_TIMING_LOAD \
{ \
  uint64_t volatile __ompvv_b = 0;\
    __ompvv_b++; \
}

#define OMPVV_START_TIMER \
  _ompvv_accum_driver = 0; \
  _ompvv_accum_kernel = 0; \
  _ompvv_accum_runtime = 0; \
  _ompvv_accum_memory = 0; \
  _ompvv_accum_others = 0; \
  CUPTI_CALL(cuptiGetTimestamp(&startTimestamp));

#define OMPVV_STOP_TIMER  \
      cuptiActivityFlushAll(0);

#define OMPVV_INIT_TEST \
  OMPVV_INFOMSG("Starting test");

#define OMPVV_REGISTER_TEST

#define OMPVV_TIMER_RESULT(clause) \
  do { \
    uint64_t stopTimestamp; \
    uint64_t testDuration, cudaDuration; \
    CUPTI_CALL(cuptiGetTimestamp(&stopTimestamp)); \
    testDuration = stopTimestamp - startTimestamp; \
    cudaDuration = _ompvv_accum_driver + _ompvv_accum_kernel + _ompvv_accum_runtime + _ompvv_accum_memory + _ompvv_accum_others; \
    printf("TEST_END \t %s \t %lu \t %lu \t %lu\n", clause, startTimestamp, stopTimestamp, testDuration); \
    printf("TEST_SUMMARY\t%s\t%s\t%s\t%lu\t%lu\n", OMPVV_COMPILER_NAME, OMPVV_COMPILER_VERSION, clause, testDuration, cudaDuration); \
    fflus(stdout); \
  } while (0);

#endif

#endif
